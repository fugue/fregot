{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}  -- for the MonadUnify instance...
module Fregot.Eval
    ( Environment (..), builtins, rules, inputDoc, stack, ruleCache
    , comprehensionCache
    , Context, locals
    , emptyContext

    , Value
    , Document
    , Row, rowValue
    , prettyRowWithContext

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

import           Control.Arrow             ((>>>))
import           Control.Lens              (review, to, use, view, (%=), (.=),
                                            (.~), (^.), (^?))
import           Control.Monad             (void, when, (>=>))
import           Control.Monad.Extended    (forM, zipWithM_)
import           Control.Monad.Identity    (Identity (..))
import           Control.Monad.Reader      (ask, local)
import qualified Control.Monad.Stream      as Stream
import           Data.Foldable             (for_)
import           Data.Functor              (($>))
import qualified Data.HashMap.Strict       as HMS
import qualified Data.HashSet              as HS
import           Data.Int                  (Int64)
import qualified Data.List                 as L
import           Data.Maybe                (catMaybes, fromMaybe)
import qualified Data.Unification          as Unification
import qualified Data.Vector.Extended      as V
import           Fregot.Arity
import           Fregot.Builtins.Internal
import           Fregot.Compile.Package    (CompiledRule, valueToCompiledRule)
import qualified Fregot.Eval.Cache         as Cache
import           Fregot.Eval.Internal
import           Fregot.Eval.Monad
import           Fregot.Eval.Mu
import qualified Fregot.Eval.Number        as Number
import qualified Fregot.Eval.TempObject    as TempObject
import           Fregot.Eval.Value
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens
import           Fregot.PrettyPrint        ((<$$>), (<+>))
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Fregot.Tree               (Tree)
import qualified Fregot.Tree               as Tree
import           Fregot.Types.Rule         (RuleType (..))

ground :: SourceSpan -> Mu' -> EvalM Value
ground source = unMu >>> \case
    FreeM v -> do
        mbMu <- Unification.lookup v
        case mbMu of
            Nothing -> raise' source "unknown variable" $
                "Unknown variable:" <+> PP.pretty v
            Just mu -> ground source mu
    WildcardM -> raise' source "unknown variable" $
                "Unknown variable:" <+> PP.pretty (Mu WildcardM)
    GroundedM v -> pure v
    RecM v -> Value <$> traverse (ground source) v
    TreeM env _ tree -> local (const env) $
        fromMaybe emptyObject <$> groundTree source tree


groundTree :: SourceSpan -> Tree.Tree CompiledRule -> EvalM (Maybe Value)
groundTree source tree = case Tree.root tree of
    Just crule -> case crule ^. ruleKind of
        FunctionRule _ -> pure Nothing  -- Don't include functions.
        _              -> Just <$> evalCompiledRule source crule Nothing
    Nothing | null (Tree.children tree) -> pure Nothing
    Nothing -> do
        -- NOTE(jaspervdj): If there is both a rule as well as children,
        -- we'll need to do some sort of merge here.
        children <- unbranch $ mconcat $ do
            (v, child) <- Tree.children tree
            pure $ fmap ((,) (key v)) <$> groundTree source child
        fmap Just . mkObject source $ catMaybes children
  where
    key = Value . StringV . unVar


mkObject :: SourceSpan -> [(Value, Value)] -> EvalM Value
mkObject source assoc = do
    tempObj <- TempObject.new HMS.empty
    for_ assoc $ \(k, v) ->
        TempObject.write tempObj k v >>= \case
        TempObject.Inconsistent v' -> raiseInconsistentObject source k v v'
        _                          -> pure ()
    fmap (Value . ObjectV) $ TempObject.read tempObj


raiseInconsistentObject
    :: SourceSpan -> Value -> Value -> Value -> EvalM a
raiseInconsistentObject source k v v' = raise' source "inconsistent object" $
    "Object key-value pairs must be consistent, but got:" <$$>
    PP.ind (PP.pretty v) <$$>
    "And:" <$$>
    PP.ind (PP.pretty v') <$$>
    "For key:" <$$>
    PP.ind (PP.pretty k)


evalGroundTerm :: Term SourceSpan -> EvalM Value
evalGroundTerm term = evalTerm term >>= ground (term ^. termAnn)


evalTerm :: Term SourceSpan -> EvalM Mu'
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
            muValue <$> evalCompiledRule source crule (Just arg')
        _ -> do
            val <- evalTerm lhs
            idx <- evalTerm arg
            evalRefArg source val idx


evalTerm (CallT source f args) = do
    builtins' <- view builtins
    case HMS.lookup f builtins' of
        Just builtin -> do
            vargs <- mapM evalTerm args
            muValue <$> evalBuiltin source builtin vargs

        _ | NamedFunction rn <- f -> do
            mbCompiledRule <- lookupRule rn
            case mbCompiledRule of
                Just cr -> do
                    vargs <- mapM evalTerm args
                    muValue <$> evalUserFunction source cr vargs
                Nothing -> raise' source "unknown function" $
                    "Unknown function call:" <+> PP.pretty f

        _ -> raise' source "unknown function" $
            "Unknown function call:" <+> PP.pretty f


evalTerm (NameT source v) = evalName source v
evalTerm (ArrayT _ a) = do
    bs <- mapM evalTerm a
    return $ Mu $ RecM $ ArrayV $ V.fromList bs
evalTerm (SetT _ s) = do
    bs <- mapM evalGroundTerm s
    return $ muValueF $ SetV $ HS.fromList bs
evalTerm (ObjectT source o) = do
    obj <- forM o $ \(kt, vt) -> do
        key <- evalGroundTerm kt
        val <- evalGroundTerm vt
        return (key, val)
    muValue <$> mkObject source obj

evalTerm (CompT source comp) = evalComprehension source comp

evalTerm (ValueT _ v) = pure $ muValue v

evalTerm (ErrorT source) = raise' source "internal error" $
    "An error node was created during compilation so evaluation should" <+>
    "be allowed."

evalComprehension :: SourceSpan -> Comprehension SourceSpan -> EvalM Mu'
evalComprehension source = \case
    ArrayComp chead cbody -> do
        rows <- unbranch $ evalRuleBody cbody (evalTerm chead)
        return $ Mu $ RecM $ ArrayV $ V.fromList rows

    SetComp shead cbody -> do
        rows <- unbranch $ evalRuleBody cbody (evalTerm shead >>= ground source)
        return $ muValueF $ SetV $ HS.fromList rows

    ObjectComp khead vhead cbody -> do
        rows <- unbranch $ evalRuleBody cbody $
            (,) <$> evalGroundTerm khead <*> evalGroundTerm vhead
        muValue <$> mkObject source rows

-- Evaluate a comprehension and shard it by key values.
evalIndexedComprehension
    :: SourceSpan -> IndexedComprehension SourceSpan
    -> EvalM (HMS.HashMap [Value] Mu')
evalIndexedComprehension source (IndexedComprehension unique keys _ comp) = do
    cache <- view comprehensionCache
    cacheResult <- Cache.read cache unique
    case cacheResult of
        Just (Cache.Singleton hms) -> pure hms
        _ -> do
            -- Evaluate the entire thing.
            !hms <- evalComp
            Cache.writeSingleton cache unique hms
            pure hms
  where
     evalKeys = forM keys (evalVar source >=> ground source)
     evalComp = clearLocals $ case comp of
        ArrayComp chead cbody -> do
            rows <- unbranch $ evalRuleBody cbody $ do
                ks <- evalKeys
                v  <- evalTerm chead
                pure (ks, [v])
            return $ fmap (Mu . RecM . ArrayV . V.fromList) $
                HMS.fromListWith (flip (++)) rows

        SetComp shead cbody -> do
            rows <- unbranch $ evalRuleBody cbody $ do
                ks <- evalKeys
                v  <- evalTerm shead >>= ground source
                pure (ks, HS.singleton v)
            return $ fmap (muValueF . SetV) $ HMS.fromListWith (<>) rows

        ObjectComp khead vhead cbody -> do
            rows <- unbranch $ evalRuleBody cbody $ do
                ks <- evalKeys
                kv <- (,) <$> evalGroundTerm khead <*> evalGroundTerm vhead
                pure (ks, [kv])
            traverse (fmap muValue . mkObject source) $
                HMS.fromListWith (<>) rows

evalName :: SourceSpan -> Name -> EvalM Mu'
evalName source (LocalName var) = evalVar source var
evalName _source (BuiltinName "input") = muValue <$> view inputDoc
evalName _source (BuiltinName "data") = do
    env <- ask
    Mu . TreeM env mempty <$> view rules
evalName _source WildcardName = return (Mu WildcardM)
evalName source name@(BuiltinName _) =
    raise' source "type error" $
    "Builtin" <+> PP.pretty name <+> "can only be used as function"
evalName source name@(QualifiedName key) = do
    env <- ask
    case Tree.descendant key (env ^. rules) of
        Nothing -> raise' source "rule not found" $
            "Rule not found:" <+> PP.pretty name
        Just d | Just r <- Tree.root d, FunctionRule _ <- r ^. ruleKind ->
            -- We allow calling a null-ary function `report()` both
            -- as just `report` as well as `report()`
            muValue <$> evalUserFunction source r []
        Just d | Just r <- Tree.root d ->
            muValue <$> evalCompiledRule source r Nothing
        Just d ->
            -- No rule found here; return the whole tree.
            pure $! Mu $ TreeM env key d


evalVar :: SourceSpan -> Var -> EvalM Mu'
evalVar _source v = do
    lcls <- use locals
    case HMS.lookup v lcls of
        Just iv -> do
            mbVal <- Unification.lookup iv
            return $ fromMaybe (Mu (FreeM iv)) mbVal
        Nothing -> Mu . FreeM <$> toInstVar v


evalBuiltin :: SourceSpan -> Builtin Identity -> [Mu'] -> EvalM Value
evalBuiltin source builtin@(Builtin sig _ (Identity impl)) args0 = do
    -- There are two possible scenarios if we have an N-ary function, e.g.:
    --
    --     add(x, y) = z {
    --       z := x + y
    --     }
    --
    -- Either the user supplies 2 arguments, and we return the return value
    -- (`z` in the example), or the user supplies 3 arguments, and we unify
    -- the return value with the last argument.
    (args1, mbFinalArg) <- case checkArity (arity builtin) args0 of
        ArityOk args1 mbFinalArg -> pure (args1, mbFinalArg)
        ArityBad n               -> raise' source "builtin arity error" $
            "Expected " <+> PP.pretty (arity builtin) <+>
            "arguments but got" <+> PP.pretty n

    args2 <- mapM (ground source) args1
    args3 <- case toArgs sig args2 of
        Left err -> raise' source "builtin type error" $ PP.pretty err
        Right x  -> return x

    result <- fmap toVal . runBuiltinM source . Stream.coerce $ impl args3

    -- Return value depends on supplied arguments.
    case mbFinalArg of
        Nothing -> return result
        Just fa -> do
            _ <- unify source (Mu (GroundedM result)) fa
            return true


evalRefArg :: SourceSpan -> Mu' -> Mu' -> EvalM Mu'

evalRefArg _ (Mu (TreeM e p tree)) idx
        | Nothing <- Tree.root tree, Just s <- muToString idx =
    -- Indexing into the tree works slightly differently.
    let k = review varFromKey (mkVar s) in
    maybe cut (return . Mu . TreeM e (p <> k)) (Tree.descendant k tree)

evalRefArg _ (Mu (TreeM e p tree)) (Mu WildcardM)
        | Nothing <- Tree.root tree = branch
    [ pure $! Mu $ TreeM e (p <> review varFromKey v) t
    | (v, t) <- Tree.children tree
    ]

evalRefArg source (Mu (TreeM e p tree)) (Mu (FreeM unbound))
        | Nothing <- Tree.root tree = branch
    [ do
        _ <- Unification.bindTerm source unbound (muValueF $ StringV $ unVar v)
        pure $! Mu $ TreeM e (p <> review varFromKey v) t
    | (v, t) <- Tree.children tree
    ]

evalRefArg source indexee (Mu WildcardM) = do
    gindexee <- ground source indexee
    case unValue gindexee of
        ArrayV a  -> branch [return (muValue val) | val <- V.toList a]
        SetV s    -> branch [return (muValue val) | val <- HS.toList s]
        ObjectV o -> branch [return (muValue val) | (_, val) <- HMS.toList o]
        _         -> cut

evalRefArg source indexee (Mu (FreeM unbound)) = do
    gindexee <- ground source indexee
    case unValue gindexee of
        ArrayV a -> branch
            [ Unification.bindTerm source unbound
                (muValueF $ NumberV $ review Number.int i) >>
                return (muValue val)
            | (i, val) <- zip [0 :: Int64 ..] (V.toList a)
            ]
        SetV s -> branch
            [ Unification.bindTerm source unbound (muValue val) >> return (muValue val)
            | val <- HS.toList s
            ]
        ObjectV o -> branch
            [ Unification.bindTerm source unbound (muValue key) >> return (muValue val)
            | (key, val) <- HMS.toList o
            ]
        _ -> cut

evalRefArg source indexee idx = do
    gindexee <- ground source indexee
    case unValue gindexee of
        ObjectV o -> do
            gidx <- ground source idx
            -- NOTE(jaspervdj): We can omit some warning here.
            maybe cut (return . muValue) $! HMS.lookup gidx o

        ArrayV a -> case muToNumber idx of
            Just n  | Just i <- n ^? Number.int . to fromIntegral
                    , i >= 0 && i < V.length a ->
                return (muValue $ a V.! i)
            _ -> cut

        SetV set -> do
            -- If the LHS is a set, we just test if the index is in there.
            --
            -- NOTE(jaspervdj): Another implementation would be to loop over
            -- all elements in the set, and try to unify `idx` with those.
            -- However, the opa interpreter doesn't seem to do this.
            gidx <- ground source idx
            if gidx `HS.member` set
                then return (muValue gidx)
                else cut

        _ -> cut

-- | Returns the value of the index value (if given) as well as the result of
-- the rule.
evalCompiledRule
    :: SourceSpan
    -> Rule RuleType SourceSpan
    -> Maybe Mu'
    -> EvalM Value
evalCompiledRule callerSource crule mbIndex
    -- Cached and uncached complete definitions
    | crule ^. ruleKind == CompleteRule = pushStack $ do
        c             <- view ruleCache
        mbCacheResult <- Cache.read c ckey
        case mbCacheResult of
            Just (Cache.Singleton val) -> pure val
            _                          -> do
                val <- requireComplete (crule ^. ruleAnn) $
                    case crule ^. ruleDefault of
                        -- If there is a default, then we fill it in if the rule
                        -- yields no rows.
                        Nothing  -> snd <$> branch branches
                        Just def -> withDefault (evalGroundTerm def) $
                                    snd <$> branch branches
                Cache.writeSingleton c ckey val
                pure val

    | otherwise = pushStack $ do
        -- Figure n the keys we already know about, and whether or not we
        -- need to do more computation.
        c             <- view ruleCache
        mbCacheResult <- Cache.read c ckey
        (tempObj, partial, needCompute) <- case mbCacheResult of
            Just (Cache.Collection p) -> do
                tempObj <- TempObject.new p
                pure (tempObj, p, False)
            Just (Cache.Partial tempObj) -> do
                partial <- TempObject.read tempObj
                pure (tempObj, partial, True)
            _ -> do
                tempObj <- TempObject.new HMS.empty
                pure (tempObj, HMS.empty, True)

        -- Branches for collection rules (objects or sets).  Returns a tuple of
        -- the cached keys and values, and then a list of further branches.  The
        -- further branches will only yield keys/values that are not in the
        -- first element of the tuple.
        let more :: [EvalM (Maybe Value, Value)]
            more
                | not needCompute = []
                | otherwise       = (do
                    compute <- branches
                    pure $ do
                        (idxVal, val) <- compute
                        let (k, v) = case crule ^. ruleKind of
                                GenSetRule -> (fromMaybe val idxVal, true)
                                _          -> (fromMaybe val idxVal, val)
                        write <- TempObject.write tempObj k v
                        case write of
                            TempObject.Ok              -> pure ()
                            TempObject.Duplicate       -> cut
                            TempObject.Inconsistent v' ->
                                raiseInconsistentObject callerSource k v v'
                        return (idxVal, val)) ++ (pure $ do
                    -- After all branches have executed, indicate that the
                    -- collection is finished.
                    --
                    -- We use a heuristic to know if we have actually visited
                    -- all keys/values, false negatives are possible here but
                    -- false positives are not.
                    let visitedAll = case mbIndex of
                            Nothing             -> True
                            Just (Mu (FreeM _)) -> True
                            Just (Mu WildcardM) -> True
                            _                   -> False
                    when visitedAll $ Cache.flushCollection c ckey
                    cut)

        -- If there is an index we want to branch for every possibility.
        case crule ^. ruleKind of
            rkind | Just idx <- mbIndex -> do
                branch $
                    -- First deal with known keys/values.
                    (do
                        (k, v) <- HMS.toList partial
                        pure $ case rkind of
                            GenSetRule -> unify callerSource idx (muValue k) >> return k
                            _          -> unify callerSource idx (muValue k) >> return v) ++
                    -- Then unknown ones.
                    map (fmap snd) more

            -- Sets without an index need to evaluate to a set value.
            GenSetRule -> do
                moreElems <- unbranch $ branch $ map (fmap snd) more
                return $ Value $ SetV $
                    HMS.keysSet partial <>
                    HS.fromList moreElems

            -- Object without an index need evaluate to an object value.
            GenObjectRule -> do
                moreElems <- unbranch $ branch more
                return $ Value $ ObjectV $
                    partial <>
                    HMS.fromList [(k, v) | (Just k, v) <- moreElems]

            _ -> raise' callerSource "type error" $
                "Internal error:" <+>
                PP.pretty (crule ^. ruleName) <+>
                "is a not a rule that defines an object or set."
  where
    -- Standard branching evaluation of rule definitions; used for all
    -- evaluations.
    branches :: [EvalM (Maybe Value, Value)]
    branches = do
        def <- crule ^. ruleDefs
        pure $ evalRuleDefinition callerSource def mbIndex

    pushStack = pushRuleStackFrame
        callerSource
        (QualifiedName (crule ^. ruleKey))

    -- Cache key.
    ckey = (crule ^. rulePackage, crule ^. ruleName)


evalRuleDefinition
    :: SourceSpan -> RuleDefinition SourceSpan -> Maybe Mu'
    -> EvalM (Maybe Value, Value)
evalRuleDefinition callerSource rule mbArg =
    clearLocals $ do

        -- Optimization: If the index of the rule is just a variable or a
        -- literal, we can assign it now which may allow us to index directly
        -- into rules.  Not that not all terms are safe to evaluate, for some
        -- e.g. `foo.bar` we need to wait until after the evaluation of the
        -- body.
    let safeToEval (NameT _ _)  = True
        safeToEval (ValueT _ _) = True
        safeToEval _            = False
    mbArgVal <- case (mbArg, rule ^. ruleIndex) of
        (Just arg, Just tpl) | safeToEval tpl -> do
            tplv <- evalTerm tpl
            void $ unify callerSource arg tplv
            pure $ Just tplv
        _ -> pure Nothing

    let ret mbRet = do
            i <- case rule ^. ruleIndex of
                Nothing  -> pure Nothing
                Just idx -> do
                    case mbArg of
                        Nothing -> Just <$> evalGroundTerm idx
                        Just arg -> do
                            -- Make sure to finally unify the given index (arg)
                            -- against the computed index (idx).
                            let argv = fromMaybe arg mbArgVal
                            iv <- evalTerm idx
                            void $ unify callerSource argv iv
                            Just <$> ground (rule ^. ruleDefAnn) iv
            v <- case mbRet of
                Nothing   -> pure $ fromMaybe true i
                Just term -> evalGroundTerm term
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
    :: SourceSpan -> Rule RuleType SourceSpan -> [Mu']
    -> EvalM Value
evalUserFunction callerSource crule callerArgs =
    pushFunctionStackFrame callerSource (QualifiedName (crule ^. ruleKey)) $
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
        Nothing   -> return true
        Just term -> evalGroundTerm term

    evalFunctionDefinition def =
        clearLocals $ do
        -- TODO(jaspervdj): Check arity.
        calleeArgs <- mapM evalTerm $ fromMaybe [] (def ^. ruleArgs)
        zipWithM_ (unify callerSource) callerArgs calleeArgs
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
    []           -> return true


-- | Evaluate a literal.  If the literal does not shortcut, evaluate the next
-- evaluation using the value returned from the literal.  This will be True
-- if the literal was a negation that passed.
evalLiteral :: Literal SourceSpan -> (Value -> EvalM a) -> EvalM a
evalLiteral lit next
    | lit ^. literalNegation = do
        localWiths (lit ^. literalWith) $ negation trueish $
            evalStatement (lit ^. literalStatement) >>=
            ground (lit ^. literalAnn)
        next true
    | otherwise = do
        r <- localWiths (lit ^. literalWith) $
            evalStatement (lit ^. literalStatement) >>=
            ground (lit ^. literalAnn)
        next r
  where
    localWiths (w : ws) mx = localWith w $ localWiths ws mx
    localWiths []       mx = mx

    localWith w mx = do
        val      <- evalTerm (w ^. withAs) >>= ground (w ^. withAnn)
        modifier <- case w ^. withPath of
            InputWithPath path -> do
                input0 <- view inputDoc
                input1 <- patchObject (w ^. withAnn) path val input0
                pure $ inputDoc .~ input1
            DataWithPath path -> do
                tree0 <- view rules
                tree1 <- patchTree (w ^. withAnn) path val tree0
                pure $ rules .~ tree1
        rc <- view ruleCache >>= Cache.bump
        cc <- view comprehensionCache >>= Cache.bump
        local ((ruleCache .~ rc) . (comprehensionCache .~ cc) . modifier) mx
{-# INLINE evalLiteral #-}


evalStatement :: Statement SourceSpan -> EvalM Mu'
evalStatement (UnifyS source x y) = suspend source $ do
    xv <- evalTerm x
    yv <- evalTerm y
    _  <- unify source xv yv
    return muTrue
evalStatement (AssignS source x y) = suspend source $ do
    xv <- evalTerm x
    yv <- evalTerm y
    _  <- unify source xv yv
    return muTrue
evalStatement (TermS e) = suspend (e ^. termAnn) (evalTerm e)
evalStatement (IndexedCompS source comp) = do
    cache <- view comprehensionCache
    collection <- if cache ^. Cache.enabled then do
        -- Evaluate the entire thing.
        !index <- evalIndexedComprehension source comp
        keyVals <- forM (comp ^. indexedKeys) (evalVar source >=> ground source)
        pure $ case HMS.lookup keyVals index of
            Just mu -> mu
            Nothing -> muValueF $ case comp ^. indexedComprehension of
                ArrayComp  _ _   -> ArrayV  V.empty
                SetComp    _ _   -> SetV    HS.empty
                ObjectComp _ _ _ -> ObjectV HMS.empty
        else do
            -- Cache not enabled, use normal evaluation.
            evalComprehension source (comp ^. indexedComprehension)
    iv <- toInstVar (comp ^. indexedAssignee)
    _  <- unify source (Mu (FreeM iv)) collection
    return muTrue


unify :: SourceSpan -> Mu' -> Mu' -> EvalM Mu'
unify _source (Mu WildcardM) y = return y
unify _source x (Mu WildcardM) = return x
unify source x@(Mu (FreeM alpha)) (Mu (FreeM beta)) =
    Unification.bindVar source alpha beta $> x
unify source (Mu (FreeM alpha)) v = Unification.bindTerm source alpha v
unify source v (Mu (FreeM alpha)) = Unification.bindTerm source alpha v
unify source mu@(Mu (RecM (ArrayV larr))) (Mu (RecM (ArrayV rarr))) = do
    unifyArrayLength larr rarr
    V.zipWithM_ (unify source) larr rarr
    pure mu
unify source mu@(Mu (GroundedM (Value (ArrayV larr)))) (Mu (RecM (ArrayV rarr))) = do
    unifyArrayLength larr rarr
    V.zipWithM_ (unify source) (fmap muValue larr) rarr
    pure mu
unify source mu@(Mu (RecM (ArrayV larr))) (Mu (GroundedM (Value (ArrayV rarr)))) = do
    unifyArrayLength larr rarr
    V.zipWithM_ (unify source) larr (fmap muValue rarr)
    pure mu
unify _source mu@(Mu (GroundedM x)) (Mu (GroundedM y)) =
    if (x == y) then pure mu else cut
unify source (Mu (RecM x)) y = do
    -- This code is currently a bit stricter than it needs to be; it evaluates
    -- the entire tree in `x` while it would be good enough to "peel off one
    -- layer" (~ evaluate to whnf) and then try again.
    gx <- traverse (ground source) x
    unify source (muValue (Value gx)) y
unify source x (Mu (RecM y)) = do
    gy <- traverse (ground source) y
    unify source x (muValue (Value gy))
unify source (Mu (TreeM _ _ tree)) y = do
    gx <- maybe cut pure =<< groundTree source tree
    unify source (muValue gx) y
unify source x (Mu (TreeM _ _ tree)) = do
    gy <- maybe cut pure =<< groundTree source tree
    unify source x (muValue gy)


unifyArrayLength :: V.Vector a -> V.Vector b -> EvalM ()
unifyArrayLength larr rarr = when (V.length larr /= V.length rarr) cut


instance Unification.MonadUnify SourceSpan InstVar (Mu Environment) EvalM where
    unify = unify

    getUnification      = use unification
    putUnification u    = unification .= u
    modifyUnification f = unification %= f


-- | Updates a path in a value.  This is mainly used to implement the `with`
-- modifier.
patchObject :: SourceSpan -> [Var] -> Value -> Value -> EvalM Value
patchObject source path0 insertee = patch path0
  where
    patch []       _                   = pure insertee
    patch (v : vs) (Value (ObjectV o)) = fmap (Value . ObjectV) $! HMS.alterF
        (fmap Just . patch vs . fromMaybe emptyObject)
        (Value . StringV $ unVar v)
        o
    patch (v : _)  val                 = raise' source "`with` error" $
        "`with` statements can only be used to patch object values," <$$>
        "but found a" <+> PP.pretty (describeValue val) <+> "instead at" <$$>
        "key" <+> PP.pretty v <+> "in path" <+> PP.pretty (Nested path0)

-- | Same as `patchObject` but works for trees instead.  We try to browse down
-- into the tree as far as we can, and then convert it to an object as soon as
-- we hit a rule.
patchTree
    :: SourceSpan -> [Var] -> Value -> Tree CompiledRule
    -> EvalM (Tree CompiledRule)
patchTree source path0 insertee tree0 = case match of
    -- This matched a rule.  Reify the tree as value.  Patch the value.
    Just key@(Key kv) | Just d <- Tree.descendant key tree0 -> do
        let (pkgname, var) = toQualifiedVar key
        value  <- fromMaybe emptyObject <$> groundTree source d
        value' <- patchObject source (drop (V.length kv) path0) insertee value
        pure $ Tree.insert key
            (valueToCompiledRule source pkgname var value') tree0

    -- No rule existed in the tree.  Insert a new one.
    _ -> Tree.alterF
        (\_ ->
            let (pkgname, var) = toQualifiedVar key0 in
            pure $ Just $ valueToCompiledRule source pkgname var insertee)
        key0 tree0
  where
    -- Keys by ascending length, including the empty one.  Then find the
    -- shortest one that has a rule.
    key0  = Key $! V.fromList path0
    keys  = [Key vs | vs <- V.inits $ unKey key0]
    match = L.find (`Tree.member` tree0) keys

    -- The anonymous case is unlikely to happen and should only manifest itself
    -- in error messages since the node is still inserted into the tree at the
    -- right location.
    toQualifiedVar k = fromMaybe
        (k ^. packageNameFromKey, "anonymous") (k ^? qualifiedVarFromKey)
