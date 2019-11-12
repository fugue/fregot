{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}  -- for the MonadUnify instance...
module Fregot.Eval
    ( Environment (..), builtins, packages, inputDoc, stack, cache
    , Context, locals
    , emptyContext

    , Value
    , Document
    , Row, rowValue

    , EvalException (..)

    , EnvContext (..), ecEnvironment, ecContext
    , EvalM
    , runEvalM

    , ResumeStep
    , newResumeStep
    , Step (..)
    , runStep

    , evalVar
    , evalTerm
    , evalQuery
    ) where

import           Control.Exception         (try)
import           Control.Lens              (review, to, use, view, (%=), (.=),
                                            (.~), (^.), (^?))
import           Control.Monad             (when)
import           Control.Monad.Extended    (foldM, forM, zipWithM_)
import           Control.Monad.Identity    (Identity (..))
import           Control.Monad.Reader      (local)
import           Control.Monad.Trans       (liftIO)
import qualified Data.HashMap.Strict       as HMS
import qualified Data.HashSet              as HS
import           Data.Int                  (Int64)
import           Data.Maybe                (fromMaybe, isNothing)
import qualified Data.Unification          as Unification
import qualified Data.Vector.Extended      as V
import qualified Fregot.Compile.Package    as Package
import           Fregot.Eval.Builtins
import qualified Fregot.Eval.Cache         as Cache
import           Fregot.Eval.Monad
import qualified Fregot.Eval.Number        as Number
import           Fregot.Eval.Value
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens
import           Fregot.PrettyPrint        ((<$$>), (<+>))
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)

ground :: SourceSpan -> Value -> EvalM Value
ground source val = case val of
    FreeV v   -> do
        mbVal <- Unification.lookup v
        case mbVal of
            Nothing -> raise' source "unknown variable" $
                "Unkown variable:" <+> PP.pretty v
            Just b  -> return b
    WildcardV -> raise' source "unknown variable" $
                "Unkown variable:" <+> PP.pretty WildcardV
    _         -> return val

mkObject :: SourceSpan -> [(Value, Value)] -> EvalM Value
mkObject source assoc = fmap ObjectV $ foldM
    (\obj (k, v) -> case HMS.lookup k obj of
        Nothing           -> return $! HMS.insert k v obj
        Just v' | v' == v -> return obj
        Just v'           -> raise' source "inconsistent object" $
            "Object key-value pairs must be consistent, but got:" <$$>
            PP.ind (PP.pretty v) <$$>
            "And:" <$$>
            PP.ind (PP.pretty v') <$$>
            "For key:" <$$>
            PP.ind (PP.pretty k))
    HMS.empty assoc

evalTerm :: Term SourceSpan -> EvalM Value
evalTerm (RefT source lhs arg) = do
    mbCompiledRule <- case lhs of
        NameT _ n -> lookupRule n
        _         -> return Nothing
    case mbCompiledRule of
        -- Using a rule with an index.  This only triggers if the rule requires
        -- an argument, i.e. it is not a complete rule.
        Just crule
                | Nothing <- crule ^? ruleKind . _CompleteRule
                , Nothing <- crule ^? ruleKind . _FunctionRule -> do
            arg' <- evalTerm arg
            evalCompiledRule source crule (Just arg')
        _ -> do
            val <- evalTerm lhs
            evalRefArg source val arg

evalTerm (CallT source f args) = do
    builtins' <- view builtins
    case HMS.lookup f builtins' of
        Just builtin -> do
            vargs <- mapM evalTerm args
            evalBuiltin source builtin vargs

        _ | NamedFunction rn <- f -> do
            mbCompiledRule <- lookupRule rn
            case mbCompiledRule of
                Just cr -> do
                    vargs <- mapM evalTerm args
                    evalUserFunction source cr vargs
                Nothing -> raise' source "unknown function" $
                    "Unknown function call:" <+> PP.pretty f

        _ -> raise' source "unknown function" $
            "Unknown function call:" <+> PP.pretty f

evalTerm (NameT source v) = evalName source v
evalTerm (ScalarT _ s) = evalScalar s
evalTerm (ArrayT _ a) = do
    bs <- mapM evalTerm a
    return $ ArrayV $ V.fromList bs
evalTerm (SetT _ s) = do
    bs <- mapM evalTerm s
    return $ SetV $ HS.fromList bs
evalTerm (ObjectT source o) = do
    obj <- forM o $ \(kt, vt) -> do
        key <- evalTerm kt
        val <- evalTerm vt
        return (key, val)
    mkObject source obj

evalTerm (ArrayCompT _ chead cbody) = do
    rows <- unbranch $ evalRuleBody cbody (evalTerm chead)
    return $ ArrayV $ V.fromList rows

evalTerm (SetCompT _ shead cbody) = do
    rows <- unbranch $ evalRuleBody cbody (evalTerm shead)
    return $ SetV $ HS.fromList rows

evalTerm (ObjectCompT source khead vhead cbody) = do
    rows <- unbranch $ evalRuleBody cbody $
        (,) <$> evalTerm khead <*> evalTerm vhead
    mkObject source rows

evalName :: SourceSpan -> Name -> EvalM Value
evalName source (LocalName var) = evalVar source var
evalName _source (BuiltinName "input") = view inputDoc
evalName _source (BuiltinName "data") = return $! PackageV mempty
evalName _source WildcardName = return WildcardV
evalName source name@(BuiltinName _) =
    raise' source "type error" $
    "Builtin" <+> PP.pretty name <+> "can only be used as function"
evalName source name@(QualifiedName _pkgname _var) = do
    mbCompiledRule <- lookupRule name
    case mbCompiledRule of
        Nothing -> raise' source "rule not found" $
            "Rule not found:" <+> PP.pretty name
        Just crule | FunctionRule _ <- crule ^. ruleKind ->
            -- We allow calling a null-ary function `report()` both
            -- as just `report` as well as `report()`
            evalUserFunction source crule []
        Just crule -> evalCompiledRule source crule Nothing

evalVar :: SourceSpan -> Var -> EvalM Value
evalVar _source v = do
    lcls <- use locals
    case HMS.lookup v lcls of
        Just iv -> do
            mbVal <- Unification.lookup iv
            return $ fromMaybe (FreeV iv) mbVal
        Nothing -> FreeV <$> toInstVar v

evalBuiltin :: SourceSpan -> Builtin Identity -> [Value] -> EvalM Value
evalBuiltin source (Builtin sig (Identity impl)) args0 = do
    -- There are two possible scenarios if we have an N-ary function, e.g.:
    --
    --     add(x, y) = z {
    --       z := x + y
    --     }
    --
    -- Either the user supplies 2 arguments, and we return the return value
    -- (`z` in the example), or the user supplies 3 arguments, and we unify
    -- the return value with the last argument.
    (args1, mbFinalArg) <- case toArgs sig args0 of
        Left err -> raise' source "builtin type error" $ PP.pretty err
        Right x  -> return x

    -- Call the function.  Note that we can probably make 'try' here at the
    -- builtin a bit faster by moving it to the top-level, but we'll get less
    -- nice error messages.
    errOrResult <- liftIO $ try (impl args1)
    result      <- case errOrResult of
        Right x                     -> return $ toVal x
        Left (BuiltinException err) ->
            raise' source "builtin error" $ PP.pretty err

    -- Return value depends on supplied arguments.
    case mbFinalArg of
        Nothing -> return result
        Just fa -> do
            unify result fa
            return $ BoolV True

-- NOTE (jaspervdj): I suspect these are roughly the cases we want to care
-- about:
--
-- * indexing rules
-- * indexing "cached/precomputed" rules
-- * indexing actual values, i.e. objects and arrays
evalRefArg :: SourceSpan -> Value -> Term SourceSpan -> EvalM Value
evalRefArg source indexee refArg = do
    idx <- evalTerm refArg
    case idx of
        StringV k | PackageV pkgname <- indexee, v <- mkVar k -> do
            mbPkg <- lookupPackage pkgname
            case mbPkg of
                -- If the package exists *AND* actually has a rule with that
                -- name, we'll evaluate that name.
                Just pkg | Just _ <- Package.lookup v pkg ->
                    evalName source (QualifiedName pkgname v)
                -- Otherwise, we'll construct a further package name.  This
                -- package name does not actually need to exist (yet), since we
                -- might append more pieces to it.
                _ -> return $! PackageV (pkgname <> mkPackageName [k])

        WildcardV -> case indexee of
            ArrayV a  -> branch [return val | val <- V.toList a]
            SetV s -> branch [return val | val <- HS.toList s]
            ObjectV o -> branch [return val | (_, val) <- HMS.toList o]
            _ -> raise' source "reference error" $
                "Cannot index" <+> PP.pretty (describeValue indexee) <+>
                " using a free variable"

        FreeV unbound -> case indexee of
            ArrayV a -> branch
                [ Unification.bindTerm unbound (NumberV $ review Number.int i) >> return val
                | (i, val) <- zip [0 :: Int64 ..] (V.toList a)
                ]
            SetV s -> branch
                [ Unification.bindTerm unbound val >> return val
                | val <- HS.toList s
                ]
            ObjectV o -> branch
                [ Unification.bindTerm unbound key >> return val
                | (key, val) <- HMS.toList o
                ]
            _ -> raise' source "reference error" $
                "Cannot index" <+> PP.pretty (describeValue indexee) <+>
                " using a free variable"

        k | ObjectV o <- indexee ->
            -- NOTE(jaspervdj): We can omit some warning here.
            maybe cut return $! HMS.lookup k o

        _   | Just i <- idx ^? _NumberV . Number.int . to fromIntegral
            , ArrayV a <- indexee ->
            if i >= 0 && i < V.length a then return (a V.! i) else cut

        _ | SetV set <- indexee ->
            -- If the LHS is a set, we just test if the index is in there.
            --
            -- NOTE(jaspervdj): Another implementation would be to loop over
            -- all elements in the set, and try to unify `idx` with those.
            -- However, the opa interpreter doesn't seem to do this.
            if idx `HS.member` set
                then return idx
                else cut

        _ -> raise' source "index type error" $
            "evalRefArg: cannot index" <+> PP.pretty (describeValue indexee) <+>
            "with a" <+> PP.pretty (describeValue idx)

-- | Returns the value of the index value (if given) as well as the result of
-- the rule.
evalCompiledRule
    :: SourceSpan
    -> Rule SourceSpan
    -> Maybe Value
    -> EvalM Value
evalCompiledRule callerSource crule mbIndex = do
    c             <- view cache
    mbCacheResult <- liftIO $ Cache.read c ckey
    case crule ^. ruleKind of
        -- Cached and uncached complete definitions
        CompleteRule | Just (Cache.Singleton val) <- mbCacheResult -> pure val
        CompleteRule -> do
            val <- push $ requireComplete (crule ^. ruleAnn) $
                case crule ^. ruleDefault of
                    -- If there is a default, then we fill it in if the rule
                    -- yields no rows.
                    Nothing  -> snd <$> branch branches
                    Just def -> withDefault (evalTerm def) $
                                snd <$> branch branches
            liftIO $ Cache.writeSingleton c ckey val
            pure val

        -- Any kind of collection
        GenSetRule | isNothing mbIndex -> do
            let (partial, more) = moreBranches c mbCacheResult
            moreElems <- unbranch $ branch more
            return $ SetV $
                HMS.keysSet partial <> HS.fromList (snd <$> moreElems)
        GenObjectRule | isNothing mbIndex -> do
            let (partial, more) = moreBranches c mbCacheResult
            moreElems <- unbranch $ branch more
            return $ ObjectV $
                partial <> HMS.fromList [(k, v) | (Just k, v) <- moreElems]

        rkind | Just idx <- mbIndex -> do
            let (partial, more) = moreBranches c mbCacheResult
            branch $
                -- First deal with known keys/values.
                (do
                    (k, v) <- HMS.toList partial
                    pure $ case rkind of
                        GenSetRule -> unify idx k >> return k
                        _          -> unify idx k >> return v) ++
                -- Then unknown ones.
                map (fmap snd) more

        _ -> error "wat"
  where
    -- Update the stack
    push = pushRuleStackFrame callerSource
        (QualifiedName (crule ^. rulePackage) (crule ^. ruleName))

    -- Standard branching evaluation of rule definitions.
    branches :: [EvalM (Maybe Value, Value)]
    branches =
        [ evalRuleDefinition callerSource def mbIndex
        | def <- crule ^. ruleDefs
        ]

    -- Do more computation if necessary.
    moreBranches
        :: Cache.Cache (PackageName, Var) Value
        -> Maybe (Cache.Result Value)
        -> (HMS.HashMap Value Value, [EvalM (Maybe Value, Value)])
    moreBranches c mbCacheResult
        | not more  = (partial, [])
        | otherwise = (,) partial $ (do
            compute <- branches
            pure $ do
                (idxVal, val) <- push $ compute
                let (k, v) = case crule ^. ruleKind of
                        GenSetRule -> (fromMaybe val idxVal, BoolV True)
                        _          -> (fromMaybe val idxVal, val)
                when (k `HMS.member` partial) cut  -- Already known.
                liftIO $ Cache.writeCollection c ckey k v
                return (idxVal, val)) ++
            (pure $ do
                -- After all branches have executed, indicate that the
                -- collection is finished.
                let visitedAll = case mbIndex of
                        Nothing        -> True
                        Just (FreeV _) -> True
                        Just WildcardV -> True
                        _              -> False
                when visitedAll $ do
                    liftIO $ Cache.flushCollection c ckey
                cut)
      where
        -- Figure n the keys we already know about, and whether or not we
        -- need to do more computation -> n.
        (partial, more) = case mbCacheResult of
            Just (Cache.Collection p) -> (p, False)
            Just (Cache.Partial p)    -> (p, True)
            _                         -> (HMS.empty, True)

    -- Cache key.
    ckey = (crule ^. rulePackage, crule ^. ruleName)

evalRuleDefinition
    :: SourceSpan -> RuleDefinition SourceSpan -> Maybe Value
    -> EvalM (Maybe Value, Value)
evalRuleDefinition callerSource rule mbIndex =
    clearLocals $ do

    mbIdxVal <- case (mbIndex, rule ^. ruleIndex) of
        (Nothing, Nothing) -> return Nothing
        (Just arg, Just tpl) -> do
            tplv <- evalTerm tpl
            _    <- unify arg tplv
            return $ Just tplv
        (Just _, Nothing) -> raise' callerSource "arity problem" $
            "An argument was given for rule" <+>
            PP.pretty (rule ^. ruleDefName) <+>
            "but it does not expect one"
        (Nothing, Just tpl) -> do
            tplv <- evalTerm tpl
            return $ Just tplv

    let ret mbRet = case mbRet of
            Nothing -> do
                i <- traverse (ground (rule ^. ruleDefAnn)) mbIdxVal
                return (Nothing, fromMaybe (BoolV True) i)
            Just term -> do
                i <- traverse (ground (rule ^. ruleDefAnn)) mbIdxVal
                v <- evalTerm term
                return (i, v)

    case rule ^. ruleBodies of
        -- If there is not a single body, we probably have something like
        --
        --     a = 1
        --
        -- so we can skip directly to the return.
        []     -> ret (rule ^. ruleValue)
        bodies ->
            branch
                [ evalRuleBody body (ret $ rule ^. ruleValue)
                | body <- bodies
                ] `orElses`
            [ evalRuleBody (re ^. ruleElseBody) (ret $ re ^. ruleElseValue)
            | re <- rule ^. ruleElses
            ]

evalUserFunction
    :: SourceSpan -> Rule SourceSpan -> [Value] -> EvalM Value
evalUserFunction callerSource crule callerArgs =
    pushFunctionStackFrame callerSource
        (QualifiedName (crule ^. rulePackage) (crule ^. ruleName)) $
    case crule ^? ruleKind . _FunctionRule of
        Nothing -> raise' callerSource "type error" $
            PP.pretty (crule ^. ruleName) <+>
            "was called as function but it is not a function"
        Just _ -> requireComplete (crule ^. ruleAnn) $ branch
            [ evalFunctionDefinition def
            | def <- crule ^. ruleDefs
            ]
  where
    ret mbTerm = case mbTerm of
        Nothing   -> return (BoolV True)
        Just term -> ground (crule ^. ruleAnn) =<< evalTerm term

    evalFunctionDefinition def =
        clearLocals $ do
        -- TODO(jaspervdj): Check arity.
        calleeArgs <- mapM evalTerm $ fromMaybe [] (def ^. ruleArgs)
        zipWithM_ unify callerArgs calleeArgs
        case def ^. ruleBodies of
            -- If there is not a single body, we may have something like
            --
            --     a(x) = {x | x > 2}
            --
            -- so we can skip directly to the return.
            []     -> ret (def ^. ruleValue)
            bodies -> branch
                [ evalRuleBody body $ ret $ def ^. ruleValue
                | body <- bodies
                ] `orElses`
                [ evalRuleBody (re ^. ruleElseBody) (ret $ re ^. ruleElseValue)
                | re <- def ^. ruleElses
                ]

-- | Evaluate the rule body, then perform a continuation.
evalRuleBody :: RuleBody SourceSpan -> EvalM a -> EvalM a
evalRuleBody lits0 final = go lits0
  where
    go []           = final
    go (lit : lits) = evalLiteral lit $ \val ->
        if trueish val then go lits else cut

-- | Evaluate the statements in the query and return the value returned by the
-- last statement.
evalQuery :: Query SourceSpan -> EvalM Value
evalQuery lits0 = case reverse lits0 of
    -- Watch out for the double 'reverse'.
    (lit : lits) -> evalRuleBody (reverse lits) (evalLiteral lit return)
    []           -> return (BoolV True)

-- | Evaluate a literal.  If the literal does not shortcut, evaluate the next
-- evaluation using the value returned from the literal.  This will be True
-- if the literal was a negation that passed.
evalLiteral :: Literal SourceSpan -> (Value -> EvalM a) -> EvalM a
evalLiteral lit next
    | lit ^. literalNegation = localWiths (lit ^. literalWith) $ do
        negation trueish $
            evalStatement (lit ^. literalStatement) >>=
            ground (lit ^. literalAnn)
        next (BoolV True)
    | otherwise = localWiths (lit ^. literalWith) $ do
        v <- evalStatement $ lit ^. literalStatement
        r <- ground (lit ^. literalAnn) v
        next r
  where
    localWiths []    mx = mx
    localWiths withs mx = do
        -- Since we changed the input, we need to bump up the cache.  This will
        -- also be the case when we modify `data`.
        let updateInput input0 [] = do
                c <- view cache >>= liftIO . Cache.bump
                local (cache .~ c) $ local (inputDoc .~ input0) $ mx
            updateInput input0 (w : ws) = do
                val    <- evalTerm (w ^. withAs)
                input1 <- case updateObject (w ^. withPath) val input0 of
                    Nothing -> raise' (w ^. withAnn) "with error" $
                        "Could not update input document." <$$>
                        "Path:" <$$>
                        PP.ind (PP.pretty (Nested $ w ^. withPath))
                    Just i  -> return i
                updateInput input1 ws

        input <- view inputDoc
        updateInput input withs
{-# INLINE evalLiteral #-}

evalStatement :: Statement SourceSpan -> EvalM Value
evalStatement (UnifyS source x y) = suspend source $ do
    xv <- evalTerm x
    yv <- evalTerm y
    _  <- unify xv yv
    return $ BoolV True
evalStatement (AssignS source v x) = suspend source $ do
    xv <- evalTerm x
    iv <- toInstVar v
    unify (FreeV iv) xv
    return $ BoolV True
evalStatement (TermS e) = suspend (e ^. termAnn) (evalTerm e)

evalScalar :: Scalar -> EvalM Value
evalScalar (String t) = return $ StringV t
evalScalar (Number n) = return $ NumberV $ Number.fromScientific n
evalScalar (Bool   b) = return $ BoolV   b
evalScalar Null       = return $ NullV

unify :: Value -> Value -> EvalM ()
unify WildcardV _     = return ()
unify _ WildcardV     = return ()
unify (FreeV alpha) (FreeV beta) = Unification.bindVar alpha beta
unify (FreeV alpha) v = Unification.bindTerm alpha v
unify v (FreeV alpha) = Unification.bindTerm alpha v
unify (ArrayV larr) (ArrayV rarr)
    | V.length larr /= V.length rarr = cut
    | otherwise                      = V.zipWithM_ unify larr rarr
unify lhs rhs
    | lhs == rhs      = return ()
unify _ _             = cut

instance Unification.MonadUnify InstVar Value EvalM where
    unify = unify

    getUnification      = use unification
    putUnification u    = unification .= u
    modifyUnification f = unification %= f
