{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}  -- for the MonadUnify instance...
module Fregot.Eval
    ( Environment (..), packages, package, inputDoc, imports

    , Value
    , Document
    , Row, rowValue

    , EvalException (..)

    , EvalM
    , runEvalM

    , evalVar
    , evalExpr
    , evalTerm
    ) where

import           Control.Lens              (use, view, (%=), (.=), (.~), (^.))
import           Control.Monad.Extended    (forM, zipWithM_)
import           Control.Monad.Reader      (local)
import qualified Data.HashMap.Strict       as HMS
import qualified Data.HashSet              as HS
import           Data.Maybe                (fromMaybe, isNothing)
import qualified Data.Scientific           as Scientific
import qualified Data.Unification          as Unification
import qualified Data.Vector.Extended      as V
import           Fregot.Eval.Builtins
import           Fregot.Eval.Monad
import           Fregot.Eval.Value
import           Fregot.Prepare.AST
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)

ground :: EvalM Value -> EvalM Value
ground mval = do
    val <- mval
    case val of
        FreeV v   -> do
            mbVal <- Unification.lookup v
            case mbVal of
                Nothing -> fail $ "Unknown variable: " ++ show (PP.pretty v)
                Just b  -> return b
        WildcardV -> fail $ "Unknown variable: _"
        _         -> return val

evalExpr :: Expr SourceSpan -> EvalM Value
evalExpr (TermE _ t)      = evalTerm t
evalExpr (BinOpE _ x o y) = evalBinOp x o y

evalTerm :: Term SourceSpan -> EvalM Value
evalTerm (RefT _ lhs arg) = do
    mbCompiledRule <- case lhs of
        VarT _ v -> lookupRule [v]
        _        -> return Nothing
    case mbCompiledRule of
        -- Using a rule with an index.  This only triggers if the rule requires
        -- an argument, i.e. it is not a complete rule.
        Just crule | CompleteRule /= (crule ^. ruleKind) -> do
            arg' <- evalTerm arg
            evalCompiledRule crule (Just arg')
        _ -> do
            val <- evalTerm lhs
            evalRefArg val arg

evalTerm (CallT source f args)
    | Just builtin <- HMS.lookup f builtins = do
        vargs <- mapM evalTerm args
        evalBuiltin source builtin vargs

    | otherwise = do
        mbCompiledRule <- lookupRule f
        case mbCompiledRule of
            Just cr -> do
                vargs <- mapM evalTerm args
                evalUserFunction cr vargs
            Nothing -> fail $ "Not implemented: function call: " ++ show f

evalTerm (VarT _ v) = evalVar v
evalTerm (ScalarT _ s) = evalScalar s
evalTerm (ArrayT _ a) = do
    bs <- mapM evalExpr a
    return $ ArrayV $ V.fromList bs
evalTerm (SetT _ s) = do
    bs <- mapM evalExpr s
    return $ SetV $ HS.fromList bs
evalTerm (ObjectT _ o) = do
    obj <- forM o $ \(kt, vt) -> do
        key <- evalTerm kt
        val <- evalExpr vt
        return (key, val)

    -- TODO(jaspervdj): Check for key consistency and optimizations.
    return $ ObjectV $ V.fromList obj

evalTerm (ArrayCompT _ chead cbody) = do
    rows <- unbranch $ evalRuleBody cbody (evalTerm chead)
    return $ ArrayV $ V.fromList rows

evalTerm (SetCompT _ shead cbody) = do
    rows <- unbranch $ evalRuleBody cbody (evalTerm shead)
    return $ SetV $ HS.fromList rows

evalTerm (ObjectCompT _ khead vhead cbody) = do
    rows <- unbranch $ evalRuleBody cbody $
        (,) <$> evalTerm khead <*> evalTerm vhead
    return $ ObjectV $ V.fromList rows

evalVar :: Var -> EvalM Value
evalVar "_"     = return WildcardV
evalVar "input" = view inputDoc
evalVar v       = do
    imps <- view imports
    lcls <- use locals
    case HMS.lookup v imps of
        Just (_ann, pkgname) -> return $ PackageV pkgname
        Nothing  -> case HMS.lookup v lcls of
            Just iv -> do
                mbVal <- Unification.lookup iv
                return $ fromMaybe (FreeV iv) mbVal
            Nothing  -> do
                mbCompiledRule <- lookupRule [v]
                case mbCompiledRule of
                    Nothing    -> FreeV <$> toInstVar v
                    Just crule -> evalCompiledRule crule Nothing

evalBuiltin :: SourceSpan -> Builtin -> [Value] -> EvalM Value
evalBuiltin source (Builtin sig impl) args0 = do
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

    -- Call the function.  This is currently pure business but it will
    -- definitely involve IO at some point.
    result <- case impl args1 of
        Left err -> fail $ "evalBuiltin: failed: " ++ show err
        Right x  -> return $ toVal x

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
evalRefArg :: Value -> Term SourceSpan -> EvalM Value
evalRefArg indexee refArg = do
    idx <- evalTerm refArg
    case idx of
        StringV k | PackageV pkgname <- indexee ->
            withPackage pkgname $ evalVar (Var k)

        WildcardV -> case indexee of
            ArrayV a  -> branch [return val | val <- V.toList a]
            SetV s -> branch [return val | val <- HS.toList s]
            ObjectV o -> branch [return val | (_, val) <- V.toList o]
            _ -> fail $
                "evalRefArg: cannot index " ++ describeValue indexee ++
                " with a free variable"

        FreeV unbound -> case indexee of
            ArrayV a -> branch
                [ Unification.bindTerm unbound (NumberV $ fromIntegral i) >> return val
                | (i, val) <- zip [0 :: Int ..] (V.toList a)
                ]
            SetV s -> branch
                [ Unification.bindTerm unbound val >> return val
                | val <- HS.toList s
                ]
            ObjectV o -> branch
                [ Unification.bindTerm unbound key >> return val
                | (key, val) <- V.toList o
                ]
            _ -> fail $
                "evalRefArg: cannot index " ++ describeValue indexee ++
                " with a free variable"

        k | ObjectV o <- indexee ->
            -- NOTE(jaspervdj): We can omit some warning here.
            maybe cut return $! V.lookup k o

        NumberV n
                | Right i <- Scientific.floatingOrInteger n :: Either Double Int
                , ArrayV a <- indexee ->
            if i >= 0 && i < V.length a then
                return (a V.! i)
            else
                fail "evalRefArg: index out of bounds"

        _ | SetV set <- indexee ->
            -- If the LHS is a set, we just test if the index is in there.
            --
            -- NOTE(jaspervdj): Another implementation would be to loop over
            -- all elements in the set, and try to unify `idx` with those.
            -- However, the opa interpreter doesn't seem to do this.
            if idx `HS.member` set
                then return idx
                else cut

        _ -> fail $
            "evalRefArg: cannot index " ++ describeValue indexee ++
            " with a " ++ describeValue idx

-- | Returns the value of the index value (if given) as well as the result of
-- the rule.
evalCompiledRule
    :: Rule SourceSpan -> Maybe Value
    -> EvalM Value
evalCompiledRule crule mbIndex = case crule ^. ruleKind of
    -- Complete definitions
    CompleteRule -> requireComplete $
        case crule ^. ruleDefault of
            -- If there is a default, then we fill it in if the rule yields no
            -- rows.
            Just def -> withDefault (evalTerm def) branches
            Nothing  -> branches

    -- TODO(jaspervdj): We currently treat objects and sets the same.  This is
    -- wrong.
    _ ->
        -- If the rule takes an argument, e.g. `resources[id]`, but we refer to
        -- it without argument, e.g. `count(resources)`, we want to evaluate the
        -- document to an array again.
        (if isNothing mbIndex
            then fmap (ArrayV . V.fromList) . unbranch
            else id) $
        branches
  where
    -- Standard branching evaluation of rule definitions.
    branches = branch
        [ evalRuleDefinition def mbIndex
        | def <- crule ^. ruleDefs
        ]

evalRuleDefinition
    :: RuleDefinition SourceSpan -> Maybe Value -> EvalM Value
evalRuleDefinition rule mbIndex =
    withImports (rule ^. ruleDefImports) $
    clearLocals $
    case (mbIndex, rule ^. ruleIndex) of
        (Nothing, Nothing)   -> evalRuleBody (rule ^. ruleBody) (final Nothing)
        (Just arg, Just tpl) -> do
            tplv <- evalTerm tpl
            _    <- unify arg tplv
            evalRuleBody (rule ^. ruleBody) (final $ Just tplv)
        (Just _, Nothing) -> fail $
            "evalRuleDefinition: got argument for rule " ++
            show (PP.pretty (rule ^. ruleDefName)) ++
            " but didn't expect one"

        -- If the rule definition has an argument, but we didn't give any, we
        -- want to evaluate things anyway.
        (Nothing, Just _) -> evalRuleBody (rule ^. ruleBody) (final Nothing)
  where
    -- NOTE(jasperdj): We are using `mbIdxVal` here rather than `mbIndex` since
    -- the index might be a wildcard.
    final mbIdxVal = case rule ^. ruleValue of
        Nothing   -> ground $ return $ fromMaybe (BoolV True) mbIdxVal
        Just term -> evalTerm term

evalUserFunction
    :: Rule SourceSpan -> [Value] -> EvalM Value
evalUserFunction crule callerArgs
    | crule ^. ruleKind /= FunctionRule = fail
        "Non-function called as function"
    | otherwise = requireComplete $ branch
        [ evalFunctionDefinition def
        | def <- crule ^. ruleDefs
        ]
  where
    evalFunctionDefinition def =
        withImports (def ^. ruleDefImports) $ clearLocals $ do
            -- TODO(jaspervdj): Check arity.
            calleeArgs <- mapM evalTerm $ fromMaybe [] (def ^. ruleArgs)
            zipWithM_ unify callerArgs calleeArgs
            evalRuleBody (def ^. ruleBody) $ case def ^. ruleValue of
                Nothing   -> return (BoolV True)
                Just term -> evalTerm term

-- | Evaluate the rule body, then perform a continuation.
evalRuleBody :: RuleBody SourceSpan -> EvalM a -> EvalM a
evalRuleBody lits0 final = go lits0
  where
    go [] = final

    go (lit : lits)
        | lit ^. literalNegation = localWiths (lit ^. literalWith) $ do
            negation trueish $ ground $ evalStatement $ lit ^. literalStatement
            go lits
        | otherwise = localWiths (lit ^. literalWith) $ do
            r <- ground $ evalStatement $ lit ^. literalStatement
            if trueish r then go lits else cut

    trueish (BoolV False) = False
    trueish _             = True

    localWiths []       mx = mx
    localWiths (w : ws) mx = do
        val    <- evalTerm (w ^. withAs)
        input  <- view inputDoc
        input' <- case updateObject (w ^. withPath) val input of
            Nothing -> fail $ "Issue updating input doc"
            Just i  -> return i

        local (inputDoc .~ input') $ localWiths ws mx

evalStatement :: Statement SourceSpan -> EvalM Value
evalStatement (UnifyS _ x y) = do
    xv <- evalExpr x
    yv <- evalExpr y
    _  <- unify xv yv
    return $ BoolV True
evalStatement (AssignS _ v x) = do
    xv <- evalExpr x
    iv <- toInstVar v
    unify (FreeV iv) xv
    return xv
evalStatement (ExprS e) = evalExpr e

evalScalar :: Scalar a -> EvalM Value
evalScalar (String t) = return $ StringV t
evalScalar (Number t) = return $ NumberV t
evalScalar (Bool   b) = return $ BoolV   b
evalScalar Null       = return $ NullV

evalBinOp :: Expr SourceSpan -> BinOp -> Expr SourceSpan -> EvalM Value
evalBinOp x op y = do
    xv <- evalExpr x
    yv <- evalExpr y
    case (xv, op, yv) of
        (_, EqualO, _)    -> return $! BoolV $! xv == yv
        (_, NotEqualO, _) -> return $! BoolV $! xv /= yv
        (NumberV xn, LessThanO, NumberV yn) ->
            return $! BoolV $! xn < yn
        (NumberV xn, LessThanOrEqualO, NumberV yn) ->
            return $! BoolV $! xn <= yn
        (NumberV xn, GreaterThanO, NumberV yn) ->
            return $! BoolV $! xn > yn
        (NumberV xn, GreaterThanOrEqualO, NumberV yn) ->
            return $! BoolV $! xn >= yn
        (NumberV xn, PlusO, NumberV yn) ->
            return $! NumberV $! xn + yn
        (NumberV xn, MinusO, NumberV yn) ->
            return $! NumberV $! xn - yn
        (NumberV xn, TimesO, NumberV yn) ->
            return $! NumberV $! xn * yn
        (NumberV xn, DivideO, NumberV yn) ->
            return $! NumberV $! xn / yn
        _ -> fail $
            "evalBinOp: invalid arguments for " ++ show op ++
            ": " ++ describeValue xv ++ ", " ++ describeValue yv

unify :: Value -> Value -> EvalM ()
unify WildcardV _     = return ()
unify _ WildcardV     = return ()
unify (FreeV alpha) (FreeV beta) = Unification.bindVar alpha beta
unify (FreeV alpha) v = Unification.bindTerm alpha v
unify v (FreeV alpha) = Unification.bindTerm alpha v
unify lhs rhs
    | lhs == rhs      = return ()
unify _ _             = cut

instance Unification.MonadUnify InstVar Value EvalM where
    unify = unify

    getUnification      = use unification
    putUnification u    = unification .= u
    modifyUnification f = unification %= f
