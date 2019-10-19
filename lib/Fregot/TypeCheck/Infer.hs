{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Fregot.TypeCheck.Infer
    ( TypeError (..)
    , SourceType

    , InferEnv (..), ieBuiltins, ieDependencies
    , InferM
    , runInfer

    , inferRule
    , inferQuery
    , inferTerm
    ) where

import           Control.Lens                  (forOf_, view, (&), (.~), (^.))
import           Control.Lens.TH               (makeLenses, makePrisms)
import           Control.Monad                 (forM, join)
import           Control.Monad.Except.Extended (catching, throwError)
import           Control.Monad.Parachute       (ParachuteT, fatal)
import           Control.Monad.Reader          (ReaderT, ask, runReaderT)
import           Control.Monad.State.Strict    (StateT, evalStateT, get, modify,
                                                put)
import           Data.Bifunctor                (bimap, first, second)
import           Data.Either                   (partitionEithers)
import           Data.Foldable                 (for_)
import qualified Data.HashMap.Strict           as HMS
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
import           qualified Fregot.Prepare.Package        as Package
import           Fregot.PrettyPrint            ((<+>), (<+>?))
import qualified Fregot.PrettyPrint            as PP
import           Fregot.Sources.SourceSpan     (SourceSpan)
import qualified Fregot.TypeCheck.Builtins     as B
import           Fregot.TypeCheck.Types        (RuleType (..), Type)
import qualified Fregot.TypeCheck.Types        as Types

type SourceType = (Type, NonEmpty SourceSpan)

data TypeError
    = UnboundVars (HMS.HashMap UnqualifiedVar SourceSpan)
    | UnboundName Name SourceSpan
    | NoUnify (Maybe SourceSpan) SourceType SourceType
    | ArityMismatch SourceSpan Function Int Int
    | CannotRef SourceSpan SourceType
    | BuiltinTypeError SourceSpan PP.SemDoc
    | InternalError String

data InferEnv = InferEnv
    { _ieBuiltins     :: Builtins Proxy
    , _ieDependencies :: HMS.HashMap PackageName (Package RuleType)
    }

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
            "The variable" <+> PP.code (PP.pretty v) <+> "is not defined"

    UnboundName name source -> Error.mkError sub source "Unbound name" $
        "The name" <+> PP.code (PP.pretty name) <+> "is not defined"

    NoUnify mbSource (τ, source NonEmpty.:| _) (σ, _) ->
        Error.mkError sub (fromMaybe source mbSource) "Unification error" $
            "Could not unify type" <+> PP.code (PP.pretty τ) <+>
            "with" <+> PP.code (PP.pretty σ)

    ArityMismatch source name expect got -> Error.mkError sub source
        "Arity mismatch" $
        "The function" <+> PP.code (PP.pretty name) <+> "takes" <+>
        PP.pretty expect <+> "arguments but" <+> PP.pretty got <+> "were given"

    CannotRef source (τ, _) -> Error.mkError sub source
        "Not indexable" $
        "Cannot index the type" <+> PP.code (PP.pretty τ) <+>?
        (case τ of
            Types.Object _ -> Just $ "with the given key"
            _              -> Nothing)

    BuiltinTypeError source doc -> Error.mkError sub source
        "Builtin type error" doc

    InternalError msg -> Error.mkErrorNoMeta sub $ PP.pretty msg
  where
    sub = "typecheck"

-- | Run some code, but then discard the additional results of the unification.
isolateUnification :: InferM a -> InferM a
isolateUnification mx = do
    state <- get
    x     <- mx
    put state
    return x

runInfer :: Monad m => InferEnv -> InferM a -> ParachuteT Error m a
runInfer env mx =
    let errOrA = evalStateT (runReaderT mx env) Unify.empty in
    either (fatal . fromTypeError) return errOrA

inferRule :: Rule' -> InferM (Rule Types.RuleType SourceSpan)
inferRule rule = case rule ^. ruleKind of
    FunctionRule arity ->
        -- TODO (jaspervdj): According to the current plan, functions will be
        -- skipped right now and inferred in an "inlined" way.
        pure $ rule & ruleInfo .~ Types.FunctionType arity

    -- TODO(jaspervdj): Here, we need to assign types to the arguments as well
    -- as the return value.  According to the current plan, functions will be
    -- skipped right now and inferred in an "inlined" way.
    --
    -- We'll want to do some concatenation here in addition to the current
    -- traversal.
    CompleteRule -> do
        rdefTypes <- forM (rule ^. ruleDefs) $ \rdef -> inferRuleDefinition rdef
        defType   <- traverse inferTerm (rule ^. ruleDefault)
        let retType = Types.mergeTypes $ fmap fst $
                maybeToList defType ++ map snd rdefTypes
        pure $ rule & ruleInfo .~ Types.CompleteRuleType retType

    GenSetRule -> do
        idxValTys <- forM (rule ^. ruleDefs) inferRuleDefinition
        let idxType = Types.mergeTypes $ map fst $ catMaybes $ map fst idxValTys
        pure $ rule & ruleInfo .~ Types.GenSetRuleType idxType

    GenObjectRule -> do
        (ixs, vals) <- unzip <$> forM (rule ^. ruleDefs) inferRuleDefinition
        let idxType = Types.mergeTypes $ map fst $ catMaybes ixs
            valType = Types.mergeTypes $ map fst vals
            objType = Types.ObjectType HMS.empty $ case ixs of
                [] -> Nothing
                _  -> Just (idxType, valType)
        pure $ rule & ruleInfo .~ Types.GenObjectRuleType objType

-- | Infer a rule definition and return the type of the index as well as the
-- return type.
inferRuleDefinition
    :: RuleDefinition SourceSpan -> InferM (Maybe SourceType, SourceType)
inferRuleDefinition rdef = do
    valueType <- case NonEmpty.fromList (rdef ^. ruleBodies) of
        Nothing     -> inferReturns
        Just bodies ->
            fmap mergeReturns $
            traverse (\body -> isolateUnification $ do
                inferRuleBody body
                inferReturns)
                bodies

    -- TODO(jaspervdj): Deal with elses, values, etc.
    pure valueType
  where
    bool = (Types.Boolean, NonEmpty.singleton (rdef ^. ruleDefAnn))

    inferReturns :: InferM (Maybe SourceType, SourceType)
    inferReturns = do
        valTy   <- maybe (pure bool) inferTerm (rdef ^. ruleValue)
        indexTy <- traverse inferTerm (rdef ^. ruleIndex)
        return (indexTy, valTy)

    mergeReturns
        :: NonEmpty (Maybe SourceType, SourceType)
        -> (Maybe SourceType, SourceType)
    mergeReturns rets =
        bimap (fmap mergeSourceTypes . NonEmpty.catMaybes) mergeSourceTypes $
        NonEmpty.unzip rets

mergeSourceTypes :: NonEmpty SourceType -> SourceType
mergeSourceTypes stys =
    let (tys, anns) = NonEmpty.unzip stys in
    (Types.mergeTypes (NonEmpty.toList tys), join anns)

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
    -- It sounds possible...
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
    builtins <- view ieBuiltins
    case HMS.lookup fun builtins of
        Nothing -> error $ "TODO: not a builtin: " ++ show fun
        Just b  -> inferBuiltin source fun b args

inferTerm (NameT source WildcardName) =
    pure (Types.Any, NonEmpty.singleton source)

inferTerm (NameT source (BuiltinName _)) =
    -- TODO(jaspervdj); BuiltinName will be "data" or "input", perhaps there
    -- should be an Enum type?
    pure (Types.Any, NonEmpty.singleton source)

inferTerm (NameT source (LocalName var)) = do
    mbRes <- Unify.lookup var
    case mbRes of
        Nothing -> throwError $ UnboundVars (HMS.singleton var source)
        Just ty -> return ty

inferTerm (NameT source name@(QualifiedName pkg var)) = do
    env <- ask
    if  | Just package <- HMS.lookup pkg (env ^. ieDependencies)
        , Just rule <- Package.lookup var package -> pure
            (Types.ruleTypeToType (rule ^. ruleInfo), NonEmpty.singleton source)
        | otherwise -> throwError $ UnboundName name source

inferTerm (ArrayT source items) = do
    tys <- traverse inferTerm items
    pure $ maybe
        (Types.Array Types.Any, NonEmpty.singleton source)
        (first Types.Array . mergeSourceTypes)
        (NonEmpty.fromList tys)

inferTerm (SetT source items) = do
    tys <- traverse inferTerm items
    pure $ maybe
        (Types.Set Types.Any, NonEmpty.singleton source)
        (first Types.Set . mergeSourceTypes)
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
        ( Types.Object Types.ObjectType
            { Types._otStatic  = HMS.fromList $ second fst <$> scalars
            , Types._otDynamic = case dynamics of
                []  -> Nothing
                _   -> Just
                    ( Types.mergeTypes $ fst . fst <$> dynamics
                    , Types.mergeTypes $ fst . snd <$> dynamics
                    )
            }
        , NonEmpty.singleton source
        )

inferTerm (ArrayCompT source headTerm body) = do
    headTy <- isolateUnification $ do
        inferRuleBody body
        inferTerm headTerm
    pure (Types.Array (fst headTy), NonEmpty.singleton source)

inferTerm (SetCompT source headTerm body) = do
    headTy <- isolateUnification $ do
        inferRuleBody body
        inferTerm headTerm
    pure (Types.Set (fst headTy), NonEmpty.singleton source)

inferTerm (ObjectCompT source keyTerm valueTerm body) = do
    (keyTy, valueTy) <- isolateUnification $ do
        inferRuleBody body
        (,) <$> inferTerm keyTerm <*> inferTerm valueTerm
    let objTy = Types.objectOf (fst keyTy) (fst valueTy)
    pure (objTy, NonEmpty.singleton source)

inferTerm (RefT source lhs rhs) = do
    lhsTy <- inferTerm lhs
    case lhsTy of
        (Types.Array itemTy, _) -> do
            unifyTermType source rhs (Types.Number, NonEmpty.singleton source)
            return (itemTy, NonEmpty.singleton source)
        (Types.Set elemTy, _) -> do
            unifyTermType source rhs (elemTy, NonEmpty.singleton source)
            return (Types.Boolean, NonEmpty.singleton source)
        (Types.Object objTy, _)
            -- In case the index is a scalar, and it's present in the object
            -- type, we can return a very granular type.
            | ScalarT _ rhsScalar <- rhs
            , Just specific <- HMS.lookup rhsScalar (objTy ^. Types.otStatic) ->
                return (specific, NonEmpty.singleton source)

            -- Otherwise we need to look at the dynamic part.
            | Just (dynKeyTy, dynValTy) <- objTy ^. Types.otDynamic -> do
                unifyTermType source rhs (dynKeyTy, NonEmpty.singleton source)
                return (dynValTy, NonEmpty.singleton source)

            -- There is no static or dynamic part that matches, this is either
            -- an internal error or a known empty object.
            | otherwise -> throwError $ CannotRef source lhsTy

        (Types.Any, _) -> do
            unifyTermType source rhs (Types.Any, NonEmpty.singleton source)
            return (Types.Any, NonEmpty.singleton source)

        (Types.Or _ _, _) ->
            -- TODO(jaspervdj): We _can_ support this but it requires a little
            -- effort; we need to merge all the possible types, try indexing
            -- into the possibly present array/set/object, and then merge the
            -- results.
            throwError $ CannotRef source lhsTy

        (Types.Null,    _) -> throwError $ CannotRef source lhsTy
        (Types.Number,  _) -> throwError $ CannotRef source lhsTy
        (Types.String,  _) -> throwError $ CannotRef source lhsTy
        (Types.Boolean, _) -> throwError $ CannotRef source lhsTy
        (Types.Empty,   _) -> throwError $ CannotRef source lhsTy

inferScalar :: Scalar -> Type
inferScalar = \case
    String _ -> Types.String
    Number _ -> Types.Number
    Bool   _ -> Types.Boolean
    Null     -> Types.Null

unifyTermTerm
    :: SourceSpan -> Term SourceSpan -> Term SourceSpan -> InferM ()
unifyTermTerm _ (NameT _ WildcardName)  _                       = return ()
unifyTermTerm _ _                       (NameT _ WildcardName)  = return ()
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
unifyTermType _source (NameT _ WildcardName)  _ = return ()
unifyTermType _source (NameT _ (LocalName α)) σ = Unify.bindTerm α σ
unifyTermType _source term                    σ = do
    τ <- inferTerm term
    unifyTypeType τ σ

unifyTypeType :: SourceType -> SourceType -> InferM ()
unifyTypeType (Types.Set τ, l) (Types.Set σ, r) = unifyTypeType (τ, l) (σ, r)
unifyTypeType (Types.Any, _)   (_, _)           = return ()
unifyTypeType (_, _)           (Types.Any, _)   = return ()
unifyTypeType (τ, l) (σ, r)
    | τ == σ    = return ()
    | otherwise = throwError $ NoUnify Nothing (τ, l) (σ, r)

inferBuiltin
    :: SourceSpan
    -> Function -> Builtin Proxy -> [Term SourceSpan]
    -> InferM SourceType
inferBuiltin
        source name
        builtin@(Builtin.Builtin (sig :: Builtin.Sig i o) ty _)
        args =
    case checkArity arity args of
        ArityBad got         -> throwError $ ArityMismatch source name arity got
        ArityOk inArgs mbOut -> do
            inferredArgs <- mapM inferTerm inArgs
            inTypes      <- toInTypes sig $ fmap fst inferredArgs
            outTy        <- ty checker inTypes

            case mbOut of
                Nothing -> return (outTy, NonEmpty.singleton source)
                Just o  -> do
                    unifyTermType source o (outTy, NonEmpty.singleton source)
                    return (Types.Boolean, NonEmpty.singleton source)
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
        , B.bcError = \err -> throwError $ BuiltinTypeError source err
        }

