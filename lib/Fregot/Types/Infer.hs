{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Fregot.Types.Infer
    ( TypeError (..), _UnboundVars
    , SourceType

    , InferEnv (..), ieBuiltins, ieDependencies, ieInferClosures
    , emptyInferEnv
    , InferM
    , runInfer
    , evalInfer
    , setInferContext

    , inferRule
    , inferRuleBody
    , inferQuery
    , inferLiteral
    , inferTerm
    ) where

import           Control.Lens                  (forOf_, review, view, (&), (.~),
                                                (^.), (^?))
import           Control.Lens.Extras           (is)
import           Control.Lens.TH               (makeLenses, makePrisms)
import           Control.Monad                 (forM, join, unless, void,
                                                zipWithM_)
import           Control.Monad.Except.Extended (catchError, catching,
                                                throwError)
import           Control.Monad.Parachute       (ParachuteT, fatal)
import           Control.Monad.Reader          (ReaderT, ask, runReaderT)
import           Control.Monad.State.Strict    (StateT, get, modify, put,
                                                runStateT)
import           Data.Bifunctor                (bimap, first, second)
import           Data.Either                   (partitionEithers)
import           Data.Foldable                 (for_)
import qualified Data.HashMap.Strict           as HMS
import qualified Data.HashSet                  as HS
import qualified Data.Kleene                   as K
import           Data.List.NonEmpty.Extended   (NonEmpty (..))
import qualified Data.List.NonEmpty.Extended   as NonEmpty
import           Data.Maybe                    (catMaybes, fromMaybe,
                                                maybeToList)
import           Data.Proxy                    (Proxy)
import           Data.Traversable              (for)
import qualified Data.Unification              as Unify
import           Fregot.Arity
import           Fregot.Error                  (Error)
import qualified Fregot.Error                  as Error
import           Fregot.Eval.Builtins          (Builtin, Builtins)
import qualified Fregot.Eval.Builtins          as Builtin
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Package        (Package)
import qualified Fregot.Prepare.Package        as Package
import           Fregot.PrettyPrint            ((<+>), (<+>?))
import qualified Fregot.PrettyPrint            as PP
import           Fregot.Sources.SourceSpan     (SourceSpan)
import qualified Fregot.Types.Builtins         as B
import           Fregot.Types.Internal         (Type, (∩), (⊆))
import qualified Fregot.Types.Internal         as Types
import           Fregot.Types.Rule             (RuleType (..))
import qualified Fregot.Types.Rule             as Types
import qualified Fregot.Types.Value            as Types

type SourceType = (Type, NonEmpty SourceSpan)

data TypeError
    = UnboundVars (HMS.HashMap UnqualifiedVar SourceSpan)
    | UnboundName Name SourceSpan
    | NoUnify (Maybe SourceSpan) SourceType SourceType
    | NoSubset SourceSpan Type Type
    | ArityMismatch SourceSpan Function Int Int
    | CannotRef SourceSpan SourceType
    | CannotCall SourceSpan Function
    | BuiltinTypeError SourceSpan PP.SemDoc
    | InternalError PP.SemDoc

data InferEnv = InferEnv
    { _ieBuiltins      :: Builtins Proxy
    , _ieDependencies  :: HMS.HashMap PackageName (Package RuleType)
    , _ieInferClosures :: Bool
    }

emptyInferEnv :: InferEnv
emptyInferEnv = InferEnv HMS.empty HMS.empty True

type InferState = Unify.Unification UnqualifiedVar SourceType

type InferM = ReaderT InferEnv (StateT InferState (Either TypeError))

$(makePrisms ''TypeError)
$(makeLenses ''InferEnv)

instance Unify.MonadUnify UnqualifiedVar SourceType InferM where
    unify             = unifyTypeType
    getUnification    = get
    putUnification    = put
    modifyUnification = modify

fromTypeError :: TypeError -> Error
fromTypeError = \case

    UnboundVars vars -> Error.mkMultiError sub "Unbound variables" $ do
        (v, source) <- HMS.toList vars
        return $ (,) source $
            "The variable" <+> PP.code (PP.pretty v) <+> "is referenced," <+>
            "but it is never assigned a value"

    UnboundName name source -> Error.mkError sub source "Unbound name" $
        "The name" <+> PP.code (PP.pretty name) <+> "is not defined"

    NoUnify mbSource (τ, source NonEmpty.:| _) (σ, _) ->
        Error.mkError sub (fromMaybe source mbSource) "Unification error" $
            "Could not unify type" <+> PP.code (PP.pretty τ) <+>
            "with" <+> PP.code (PP.pretty σ)

    NoSubset source σ τ ->
        Error.mkError sub source "Subtype error" $
            "Cannot use type" <+> PP.code (PP.pretty σ) <+>
            "as" <+> PP.code (PP.pretty τ)

    ArityMismatch source name expect got -> Error.mkError sub source
        "Arity mismatch" $
        "The function" <+> PP.code (PP.pretty name) <+> "takes" <+>
        PP.pretty expect <+> "arguments but" <+> PP.pretty got <+> "were given"

    CannotRef source (τ, _) -> Error.mkError sub source
        "Not indexable" $
        "Cannot index the type" <+> PP.code (PP.pretty τ) <+>?
        (if is (Types.singleton . Types._Object) τ
            then Just $ "with the given key"
            else Nothing)

    CannotCall source fun -> Error.mkError sub source
        "Not a function" $
        PP.code (PP.pretty fun) <+> "cannot be called as a function"

    BuiltinTypeError source doc -> Error.mkError sub source
        "Builtin type error" doc

    InternalError msg -> Error.mkErrorNoMeta sub msg
  where
    sub = "typecheck"

-- | Run some code, but then discard the additional results of the unification
-- and restore the original state.
isolateUnification :: InferM a -> InferM a
isolateUnification mx = do
    state <- get
    x     <- mx
    put state
    return x

-- | Run the unification in a completely empty state, and then restore the
-- original state.
blankUnification :: InferM a -> InferM a
blankUnification mx = isolateUnification (put Unify.empty >> mx)

getRule
    :: SourceSpan -> PackageName -> Var
    -> InferM (Rule Types.RuleType SourceSpan)
getRule source pkg var = do
    env <- ask
    maybe
        (throwError $ UnboundName (QualifiedName pkg var) source)
        return $ do
        package <- HMS.lookup pkg (env ^. ieDependencies)
        Package.lookup var package

runInfer
    :: Monad m => InferEnv -> InferM a -> ParachuteT Error m (a, InferState)
runInfer env mx =
    let errOrX = runStateT (runReaderT mx env) Unify.empty in
    either (fatal . fromTypeError) pure errOrX

evalInfer :: Monad m => InferEnv -> InferM a -> ParachuteT Error m a
evalInfer env = fmap fst . runInfer env

setInferContext :: SourceSpan -> Types.TypeContext -> InferM ()
setInferContext source ctx = for_ (HMS.toList ctx) $ \(var, ty) ->
    Unify.bindTerm var (ty, NonEmpty.singleton source)

inferRule :: Rule' -> InferM (Rule Types.RuleType SourceSpan)
inferRule rule = case rule ^. ruleKind of
    -- TODO(jaspervdj): Here, we need to assign types to the arguments as well
    -- as the return value.  According to the current plan, functions will be
    -- skipped right now and inferred in an "inlined" way.
    --
    -- We'll want to do some concatenation here in addition to the current
    -- traversal.
    kind | kind == CompleteRule || kind == FunctionRule 0 -> do
        rdefTypes <- forM (rule ^. ruleDefs) $ \rdef -> inferRuleDefinition rdef
        defType   <- traverse inferTerm (rule ^. ruleDefault)
        let retType = Types.unions $ fmap fst $
                maybeToList defType ++ map snd rdefTypes
        pure $ rule & ruleInfo .~ Types.CompleteRuleType retType

    FunctionRule arity -> do
        -- We do a pre-flight check using 'Types.unknown', since we don't know
        -- the actual types this will be called with.
        for_ (rule ^. ruleDefs) $ \rdef -> isolateUnification $ do
            let argTerms = fromMaybe [] $ rdef ^. ruleArgs
            zipWithM_ (unifyTermType (rdef ^. ruleDefAnn)) argTerms $ repeat $
                (Types.unknown, NonEmpty.singleton (rdef ^. ruleDefAnn))
            inferRuleDefinition rdef

        pure $ rule & ruleInfo .~ Types.FunctionType arity

    GenSetRule -> do
        idxValTys <- forM (rule ^. ruleDefs) inferRuleDefinition
        let idxType = Types.unions $ map fst $ catMaybes $ map fst idxValTys
        pure $ rule & ruleInfo .~ Types.GenSetRuleType idxType

    GenObjectRule -> do
        (ixs, vals) <- unzip <$> forM (rule ^. ruleDefs) inferRuleDefinition
        let idxType = Types.unions $ map fst $ catMaybes ixs
            valType = Types.unions $ map fst vals
            objType = Types.Obj HMS.empty $ case ixs of
                [] -> Nothing
                _  -> Just (idxType, valType)
        pure $ rule & ruleInfo .~ Types.GenObjectRuleType objType

    _ -> throwError $ InternalError
        -- Absurd: the 'kind' rule is exhaustive but GHC can't tell.
        "inferRule with absurd rule kind"

-- | Ad-hoc datatype for the branches in 'inferRuleDefinition'.
data RuleDefBranch = RuleDefBranch
    { rdbBody  :: !(Maybe (RuleBody SourceSpan))
    , rdbIndex :: !(Maybe (Term SourceSpan))
    , rdbValue :: !(Maybe (Term SourceSpan))
    }

-- | Infer a rule definition and return the type of the index as well as the
-- return type.
inferRuleDefinition
    :: RuleDefinition SourceSpan -> InferM (Maybe SourceType, SourceType)
inferRuleDefinition rdef =
    fmap mergeBranchReturns $
    traverse inferBranch branches
  where
    bool = (Types.boolean, NonEmpty.singleton (rdef ^. ruleDefAnn))

    -- This constructs a list of ALL rule bodies (normal as well as 'else' and
    -- their respective return values).
    branches :: NonEmpty RuleDefBranch
    branches = case NonEmpty.fromList (rdef ^. ruleBodies) of
        Nothing -> NonEmpty.singleton $
            RuleDefBranch Nothing (rdef ^. ruleIndex) (rdef ^. ruleValue)
        Just bodies ->
            (do
                body <- bodies
                pure $ RuleDefBranch
                    (Just body) (rdef ^. ruleIndex) (rdef ^. ruleValue))
            NonEmpty.++:
            (do
                els <- rdef ^. ruleElses
                pure $ RuleDefBranch (Just (els ^. ruleElseBody))
                    (rdef ^. ruleIndex) (els ^. ruleElseValue))

    -- Infer a single branch.
    inferBranch :: RuleDefBranch -> InferM (Maybe SourceType, SourceType)
    inferBranch rdb = isolateUnification $ do
        for_ (rdbBody rdb) inferRuleBody
        valTy   <- maybe (pure bool) inferTerm (rdbValue rdb)
        indexTy <- traverse inferTerm (rdbIndex rdb)
        return (indexTy, valTy)

    -- Merge branches.
    mergeBranchReturns
        :: NonEmpty (Maybe SourceType, SourceType)
        -> (Maybe SourceType, SourceType)
    mergeBranchReturns rets =
        bimap (fmap mergeSourceTypes . NonEmpty.catMaybes) mergeSourceTypes $
        NonEmpty.unzip rets

mergeSourceTypes :: NonEmpty SourceType -> SourceType
mergeSourceTypes stys =
    let (tys, anns) = NonEmpty.unzip stys in
    (Types.unions (NonEmpty.toList tys), join anns)

inferRuleBody :: RuleBody SourceSpan -> InferM ()
inferRuleBody body =
    -- Propagating the bound variables is really all that matters here.
    for_ body inferLiteral

inferQuery :: Query SourceSpan -> InferM ()
inferQuery = inferRuleBody

inferLiteral
    :: Literal SourceSpan -> InferM ()
inferLiteral lit = do
    -- TODO(jaspervdj): Can the with parts decide what e.g. `input` looks like?
    -- It sounds possible...  However, it requires to also check the rules we
    -- call using this 'with' in an "inlined" manner, just like we currently do
    -- functions.
    forOf_ (literalWith . traverse . withAs) lit
        (isolateUnification . inferTerm)

    -- In case we have a negative literal here, what we want to do is throw away
    -- the variables that were bound by it.
    _ <- (if lit ^.literalNegation then isolateUnification else id) $
        inferStatement (lit ^. literalStatement)

    return ()

inferStatement
    :: Statement SourceSpan -> InferM ()
inferStatement = \case
    TermS t -> () <$ inferTerm t
    AssignS _source v t -> do
        tty <- inferTerm t
        Unify.bindTerm v tty
    UnifyS source l r -> unifyTermTerm source l r

inferTerm
    :: Term SourceSpan -> InferM SourceType

inferTerm (ScalarT source scalar) =
    return $ (, NonEmpty.singleton source) $ inferScalar scalar

inferTerm (CallT source fun args) = do
    let cannotCall = throwError $ CannotCall source fun
    builtins <- view ieBuiltins
    case HMS.lookup fun builtins of
        Just b  -> inferBuiltin source fun b args
        Nothing -> case fun of
            OperatorFunction o -> throwError $ InternalError $
                "builtin for operator" <+> PP.pretty o <+> "not found"
            NamedFunction (QualifiedName pkg var) -> do
                rule <- getRule source pkg var
                inferUserFunction source fun rule args
            NamedFunction WildcardName    -> cannotCall
            NamedFunction (LocalName _)   -> cannotCall
            NamedFunction (BuiltinName _) -> cannotCall

inferTerm (NameT source WildcardName) =
    pure (Types.any, NonEmpty.singleton source)

inferTerm (NameT source (BuiltinName _)) =
    -- TODO(jaspervdj); BuiltinName will be "data" or "input", perhaps there
    -- should be an Enum type?
    pure (Types.unknown, NonEmpty.singleton source)

inferTerm (NameT source (LocalName var)) = do
    mbRes <- Unify.lookup var
    case mbRes of
        Nothing -> throwError $ UnboundVars (HMS.singleton var source)
        Just ty -> return ty

inferTerm (NameT source (QualifiedName pkg var)) = do
    rule <- getRule source pkg var
    pure (Types.ruleTypeToType (rule ^. ruleInfo), NonEmpty.singleton source)

inferTerm (ArrayT source items) = do
    tys <- traverse inferTerm items
    pure $ maybe
        (Types.arrayOf Types.empty, NonEmpty.singleton source)
        (first Types.arrayOf . mergeSourceTypes)
        (NonEmpty.fromList tys)

inferTerm (SetT source items) = do
    tys <- traverse inferTerm items
    pure $ maybe
        (Types.setOf Types.empty, NonEmpty.singleton source)
        (first Types.setOf . mergeSourceTypes)
        (NonEmpty.fromList tys)

inferTerm (ObjectT source obj) = do
    scalarsOrDynamics <- for obj $ \(keyTerm, valueTerm) -> do
        valueType <- inferTerm valueTerm
        case keyTerm of
            ScalarT _ scalar -> return (Left (scalar, valueType))
            _                -> Right . (, valueType) <$> inferTerm keyTerm

    let scalars  :: [(Scalar, SourceType)]
        dynamics :: [(SourceType, SourceType)]
        (scalars, dynamics) = partitionEithers scalarsOrDynamics

    return
        ( review Types.singleton $ Types.Object Types.Obj
            { Types.objStatic  = HMS.fromList $ second fst <$> scalars
            , Types.objDynamic = case dynamics of
                []  -> Nothing
                _   -> Just
                    ( Types.unions $ fst . fst <$> dynamics
                    , Types.unions $ fst . snd <$> dynamics
                    )
            }
        , NonEmpty.singleton source
        )

inferTerm (ArrayCompT source headTerm body) = do
    inferClosures <- view ieInferClosures
    if not inferClosures
        then pure (Types.unknown, NonEmpty.singleton source)
        else do
            headTy <- isolateUnification $ do
                inferRuleBody body
                inferTerm headTerm
            pure (Types.arrayOf (fst headTy), NonEmpty.singleton source)

inferTerm (SetCompT source headTerm body) = do
    inferClosures <- view ieInferClosures
    if not inferClosures
        then pure (Types.unknown, NonEmpty.singleton source)
        else do
            headTy <- isolateUnification $ do
                inferRuleBody body
                inferTerm headTerm
            pure (Types.setOf (fst headTy), NonEmpty.singleton source)

inferTerm (ObjectCompT source keyTerm valueTerm body) = do
    inferClosures <- view ieInferClosures
    if not inferClosures
        then pure (Types.unknown, NonEmpty.singleton source)
        else do
            (keyTy, valueTy) <- isolateUnification $ do
                inferRuleBody body
                (,) <$> inferTerm keyTerm <*> inferTerm valueTerm
            let objTy = Types.objectOf (fst keyTy) (fst valueTy)
            pure (objTy, NonEmpty.singleton source)

inferTerm (RefT source lhs rhs) = do
    lhsTy <- inferTerm lhs
    case lhsTy of
        (Types.Universe, _) -> do
            unifyTermType source rhs (Types.any, NonEmpty.singleton source)
            return (Types.any, NonEmpty.singleton source)

        (Types.Unknown, _) -> do
            unifyTermType source rhs (Types.unknown, NonEmpty.singleton source)
            return (Types.unknown, NonEmpty.singleton source)

        (Types.Union opts, _) -> case NonEmpty.fromList (HS.toList opts) of
            Nothing    -> throwError $ CannotRef source lhsTy
            Just nopts -> do
                (keytys, valtys) <- fmap NonEmpty.unzip $
                    mapM (inferElemRef lhsTy) nopts
                unifyTermType source rhs (mergeSourceTypes keytys)
                return $ mergeSourceTypes valtys

  where
    inferElemRef
        :: SourceType       -- ^ Original LHS type, only for errors
        -> Types.Elem Type  -- ^ Actual LHS
        -> InferM (SourceType, SourceType)  -- ^ Key type, value type

    inferElemRef _lhsTy (Types.Array itemTy) = return $ (,)
            (Types.number, NonEmpty.singleton source)
            (itemTy, NonEmpty.singleton source)

    inferElemRef _lhsTy (Types.Set elemTy) = return $ (,)
            (elemTy, NonEmpty.singleton source)
            (elemTy, NonEmpty.singleton source)

    inferElemRef lhsTy (Types.Object objTy) = case rhs of
        -- In case the index is a scalar, and it's present in the object
        -- type, we can return a very granular type.
        ScalarT ss s | Just ty <- HMS.lookup s (Types.objStatic objTy) ->
            return $ (,)
                (Types.scalarType s, NonEmpty.singleton ss)
                (ty, NonEmpty.singleton source)

        -- Otherwise we need to look at the dynamic part.
        _ | Just (dynk, dynv) <- Types.objDynamic objTy -> return $ (,)
            (dynk, NonEmpty.singleton source)
            (dynv, NonEmpty.singleton source)

        -- If there is no dynamic part, we need to unify the rhs against
        -- all the different static keys.
        _ | not (HMS.null (Types.objStatic objTy)) ->
            let (ks, vs) = unzip $ HMS.toList (Types.objStatic objTy)
                kty      = Types.unions $ map Types.scalarType ks
                vty      = Types.unions vs in
            return $ (,)
                (kty, NonEmpty.singleton source)
                (vty, NonEmpty.singleton source)

        -- There is no static or dynamic part that matches, this is
        -- either an internal error or a known empty object.
        _ -> throwError $ CannotRef source lhsTy

    inferElemRef lhsTy Types.String     = throwError $ CannotRef source lhsTy
    inferElemRef lhsTy Types.Number     = throwError $ CannotRef source lhsTy
    inferElemRef lhsTy Types.Boolean    = throwError $ CannotRef source lhsTy
    inferElemRef lhsTy Types.Null       = throwError $ CannotRef source lhsTy
    inferElemRef lhsTy (Types.Scalar _) = throwError $ CannotRef source lhsTy

inferScalar :: Scalar -> Type
inferScalar = Types.scalarType

unifyTermTerm
    :: SourceSpan -> Term SourceSpan -> Term SourceSpan -> InferM ()
unifyTermTerm _ (NameT _ WildcardName)  r                       =
    void (inferTerm r)  -- This might bind variables!
unifyTermTerm _ l                       (NameT _ WildcardName)  =
    void (inferTerm l)
unifyTermTerm _ (NameT _ (LocalName α)) (NameT _ (LocalName β)) =
    Unify.bindVar α β

unifyTermTerm source lhs rhs = catching _UnboundVars
    (do
        rhsty <- inferTerm rhs
        unifyTermType source lhs rhsty)
    (\_ -> do
        lhsty <- inferTerm lhs
        unifyTermType source rhs lhsty)


unifyTermType
    :: SourceSpan -> Term SourceSpan -> SourceType -> InferM ()

unifyTermType _source (NameT _ WildcardName) _ = return ()

unifyTermType _source (NameT _ (LocalName α)) σ = Unify.bindTerm α σ

unifyTermType source (ArrayT _ arr) (τ, s)
        | Just σ <- τ ^? Types.singleton . Types._Array =
    for_ arr $ \t -> unifyTermType source t (σ, s)

unifyTermType source (SetT _ set) (τ, s)
        | Just σ <- τ ^? Types.singleton . Types._Set =
    for_ set $ \t -> unifyTermType source t (σ, s)

unifyTermType _source term σ = do
    τ <- inferTerm term
    unifyTypeType τ σ


-- TODO(jaspervdj): we should return a refined type here.
unifyTypeType :: SourceType -> SourceType -> InferM ()

unifyTypeType (Types.Universe, _) (_, _)              = return ()
unifyTypeType (_, _)              (Types.Universe, _) = return ()

unifyTypeType (τ, l) (σ, r)
    | Just τ' <- τ ^? Types.singleton . Types._Array
    , Just σ' <- σ ^? Types.singleton . Types._Array =
        unifyTypeType (τ', l) (σ', r)

    | Just τ' <- τ ^? Types.singleton . Types._Set
    , Just σ' <- σ ^? Types.singleton . Types._Set =
        unifyTypeType (τ', l) (σ', r)

unifyTypeType (τ, l) (σ, r)
    | τ == σ                       = return ()
    | ρ <- τ ∩ σ, ρ /= Types.empty = return ()
    | otherwise                    =
        throwError $ NoUnify Nothing (τ, l) (σ, r)

inferBuiltin
    :: SourceSpan
    -> Function -> Builtin Proxy -> [Term SourceSpan]
    -> InferM SourceType
inferBuiltin
        source name
        builtin@(Builtin.Builtin (sig :: Builtin.Sig i o) ty _)
        args =
    inferCall source name arity args $ \inferredArgs -> do
        inTypes <- toInTypes sig $ fmap fst inferredArgs
        ty checker inTypes
  where
    toInTypes :: Builtin.Sig j o -> [Type] -> InferM (B.InTypes j)
    toInTypes Builtin.Out    _        = pure B.Nil
    toInTypes (Builtin.In s) (t : ts) = B.Cons t <$> toInTypes s ts
    toInTypes (Builtin.In _) []       =
        throwError $ InternalError "internal arity mismatch for inTypes"

    arity = Builtin.arity builtin

    checker = B.BuiltinChecker
        { B.bcUnify = \x y -> do
            unifyTypeType
                (x, NonEmpty.singleton source) (y, NonEmpty.singleton source)
            return x
        , B.bcSubsetOf = \σ τ ->
            -- NOTE(jaspervdj): We map 'K.Unknown' to 'True' here.
            unless (K.fromTernary True $ σ ⊆ τ) $
                throwError $ NoSubset source σ τ

        , B.bcError = \err -> throwError $ BuiltinTypeError source err

        , B.bcCatch = \mx my -> catchError mx (\_ -> my)
        }

inferUserFunction
    :: SourceSpan
    -> Function -> Rule Types.RuleType SourceSpan -> [Term SourceSpan]
    -> InferM SourceType
inferUserFunction source name rule args = do
    arity <- maybe
        (throwError $ CannotCall source name) return
        (rule ^? ruleInfo . Types._FunctionType)

    inferCall source name arity args $ \inferredArgs -> do
        -- Go through all rule definitions, and infer them in an environment
        -- where we have assigned the arguments types.
        --
        -- TODO(jaspervdj): we will want to cache this based on 'inferredArgs'.
        outTys <- forM (rule ^. ruleDefs) $ \rdef -> blankUnification $ do
            let argTerms = fromMaybe [] $ rdef ^. ruleArgs
            zipWithM_ (unifyTermType source) argTerms inferredArgs
            inferRuleDefinition rdef
        return $ Types.unions $ map (fst . snd) outTys

inferCall
    :: SourceSpan -> Function -> Int -> [Term SourceSpan]
    -> ([SourceType] -> InferM Type)
    -> InferM SourceType
inferCall source name arity args check =
    case checkArity arity args of
        ArityBad got         -> throwError $ ArityMismatch source name arity got
        ArityOk inArgs mbOut -> do
            inferredArgs <- mapM inferTerm inArgs
            outTy        <- check inferredArgs
            case mbOut of
                Nothing -> return (outTy, NonEmpty.singleton source)
                Just o  -> do
                    unifyTermType source o (outTy, NonEmpty.singleton source)
                    return (Types.boolean, NonEmpty.singleton source)
