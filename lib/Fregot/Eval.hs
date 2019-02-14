{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Eval
    ( Environment (..), packages, package

    , Value
    , Document
    , Row, rowValue

    , EvalM
    , runEvalM

    , evalVar
    , evalExpr
    , evalTerm
    ) where

import           Control.Lens               (use, view, (%=), (.~), (&), (^.),
                                             _3)
import           Control.Lens.TH            (makeLenses)
import           Control.Monad.Extended     (foldM, forM)
import           Control.Monad.Reader       (MonadReader (..), ask)
import           Control.Monad.State        (MonadState (..))
import qualified Data.DisjointSets          as DJ
import qualified Data.HashMap.Strict        as HMS
import           Data.Maybe                 (isJust)
import qualified Data.Scientific            as Scientific
import qualified Data.Vector                as V
import           Fregot.Eval.Value
import           Fregot.Interpreter.Package (Package)
import qualified Fregot.Interpreter.Package as Package
import qualified Fregot.PrettyPrint         as PP
import           Fregot.Sugar

newtype Scope = Scope {unScope :: HMS.HashMap Var Value}
    deriving (Monoid, Semigroup)

data Context = Context
    { _unification :: !(DJ.DisjointSets Var ())
    , _scope       :: !Scope
    }

$(makeLenses ''Context)

emptyContext :: Context
emptyContext = Context
    { _unification = DJ.empty
    , _scope       = mempty
    }

data Row a = Row
    { _rowContext :: !Context
    , _rowValue   :: !a
    } deriving (Functor)

$(makeLenses ''Row)

instance PP.Pretty PP.Sem a => PP.Pretty PP.Sem (Row a) where
    pretty (Row _ v) = PP.pretty v

type Document a = [Row a]

--------------------------------------------------------------------------------

data Environment = Environment
    { _packages :: !(HMS.HashMap PackageName Package)
    , _package  :: !Package
    } deriving (Show)

$(makeLenses ''Environment)

newtype EvalM a = EvalM {unBranchM :: Environment -> Context -> [Row a]}
    deriving (Functor)

instance Applicative EvalM where
    pure x = EvalM $ \_ ctx -> [Row ctx x]
    EvalM mf <*> EvalM mx = EvalM $ \rs ctx0 -> do
        row1 <- mf rs ctx0
        row2 <- mx rs (row1 ^. rowContext)
        return $! row2 & rowValue .~ ((row1 ^. rowValue) (row2 ^. rowValue))

instance Monad EvalM where
    EvalM mx >>= f = EvalM $ \rs ctx0 -> do
        Row ctx1 x <- mx rs ctx0
        unBranchM (f x) rs ctx1

instance MonadReader Environment EvalM where
    ask = EvalM $ \rs ctx -> pure $! Row ctx rs
    local l (EvalM f) = EvalM $ \rs ctx -> f (l rs) ctx

instance MonadState Context EvalM where
    get     = EvalM $ \_ ctx  -> [Row ctx ctx]
    put ctx = EvalM $ \_ _    -> [Row ctx ()]
    state f = EvalM $ \_ ctx0 -> let (x, ctx1) = f ctx0 in [Row ctx1 x]

runEvalM :: Environment -> EvalM a -> Document a
runEvalM rules0 (EvalM f) = f rules0 emptyContext

branch :: [EvalM a] -> EvalM a
branch options = EvalM $ \rs ctx -> do
    EvalM opt <- options
    opt rs ctx

cut :: EvalM a
cut = EvalM $ \_ _ -> []

negation :: (a -> Bool) -> EvalM a -> EvalM ()
negation trueish (EvalM f) = EvalM $ \rs ctx ->
    let rows = filter (\(Row _ x) -> trueish x) (f rs ctx) in
    if null rows then [Row ctx ()] else []

-- | Note that 'v' MUST be a root in the DisjointSets.
unsafeBind :: Var -> Value -> EvalM ()
unsafeBind root (FreeV alpha) =
    unification %= view _3 . DJ.union root alpha
unsafeBind v val =
    scope %= Scope . HMS.insert v val . unScope

lookupRule :: Var -> EvalM [Package.RuleDefinition]
lookupRule root = do
    env0 <- ask
    return $ Package.lookup root (env0 ^. package)

clearContext :: EvalM a -> EvalM a
clearContext mx = do
    ctx <- get
    put emptyContext
    x  <- mx
    put ctx
    return x

--------------------------------------------------------------------------------

evalExpr :: Expr a -> EvalM Value
evalExpr (TermE _ t) = evalTerm t
evalExpr (UnifyE _ x y) = do
    xv <- evalExpr x
    yv <- evalExpr y
    unify xv yv
    return $ BoolV True
evalExpr (BinOpE _ x o y) = evalBinOp x o y
evalExpr (ParensE _ e) = evalExpr e

evalTerm :: Term a -> EvalM Value
evalTerm (RefT _ _ v args) = do

    rs <- lookupRule v
    case args of
        -- Using a rule with an index.  This only triggers if the rule requires
        -- an argument.
        --
        -- TODO(jaspervdj): Add a check for consistent rule arity.
        [RefBrackArg x]
                | r0 : _ <- rs
                , isJust (r0 ^. Package.ruleDefRule . ruleHead . ruleIndex) -> do
            y <- evalTerm x
            branch $ map (evalRuleDefinition (Just y)) rs
        _ -> do
            val <- evalVar v
            foldM evalRefArg val args

evalTerm (VarT _ v)
    | unVar v == "_" = return WildcardV
    | otherwise      = evalVar v
evalTerm (ScalarT _ s) = evalScalar s
evalTerm (ArrayT _ a) = do
    bs <- mapM evalExpr a
    return $ ArrayV $ V.fromList bs
evalTerm (ObjectT _ o) = do
    obj <- forM o $ \(kt, vt) -> do
        key <- evalObjectKey kt
        case key of
            StringV txt -> do
                val <- evalExpr vt
                return (txt, val)
            _ -> fail "Unsupported object key type"
    return $ ObjectV $ HMS.fromList obj

evalVar :: Var -> EvalM Value
evalVar v = do
    uni  <- use unification
    scop <- use scope
    let (rv, ()) = DJ.root v uni
    case HMS.lookup rv (unScope scop) of
        Just val -> return val
        Nothing -> do
            rs <- lookupRule rv
            case rs of
                [] -> return (FreeV rv)
                _  -> branch $ map (evalRuleDefinition Nothing) rs

-- NOTE (jaspervdj): I suspect these are roughly the cases we want to care
-- about:
--
-- * indexing rules
-- * indexing "cached/precomputed" rules
-- * indexing actual values, i.e. objects and arrays
evalRefArg :: Value -> RefArg a -> EvalM Value
evalRefArg indexee refArg = do
    idx <- evalTerm (getRefArgTerm refArg)
    case idx of
        WildcardV -> case indexee of
            ObjectV o -> branch [return val | (_, val) <- HMS.toList o]
            ArrayV a  -> branch [return val | val <- V.toList a]
            _ -> fail $
                "evalRefArg: cannot index " ++ describeValue indexee ++
                " with a free variable"

        FreeV unbound -> case indexee of
            ObjectV o -> branch
                [ unsafeBind unbound (StringV key) >> return val
                | (key, val) <- HMS.toList o
                ]
            ArrayV a -> branch
                [ unsafeBind unbound (NumberV $ fromIntegral i) >> return val
                | (i, val) <- zip [0 :: Int ..] (V.toList a)
                ]
            _ -> fail $
                "evalRefArg: cannot index " ++ describeValue indexee ++
                " with a free variable"

        StringV txt | ObjectV o <- indexee -> case HMS.lookup txt o of
            Nothing -> fail "evalRefArg: key not found"
            Just f  -> return f

        NumberV n
                | Right i <- Scientific.floatingOrInteger n :: Either Double Int
                , ArrayV a <- indexee ->
            if i >= 0 && i < V.length a then
                return (a V.! i)
            else
                fail "evalRefArg: index out of bounds"

        _ -> fail $
            "evalRefArg: cannot index " ++ describeValue indexee ++
            " with a free variable"
  where
    getRefArgTerm (RefBrackArg t)       = t
    getRefArgTerm (RefDotArg a (Var k)) = ScalarT a (String k)

evalRuleDefinition :: Maybe Value -> Package.RuleDefinition -> EvalM Value
evalRuleDefinition mbIndex ruleDef = clearContext $ do
    case (mbIndex, rule ^. ruleHead . ruleIndex) of
        (Nothing, Nothing)   -> go (rule ^. ruleBody)
        (Just arg, Just tpl) -> do
            tplv <- evalTerm tpl
            unify arg tplv
            go (rule ^. ruleBody)
        (Just _, Nothing) -> fail $
            "evalRuleDefinition: got argument for rule " ++
            show (PP.pretty (rule ^. ruleHead . ruleName)) ++
            " but didn't expect one"
        (Nothing, Just _) -> fail $
            "other arity error"
  where
    rule = ruleDef ^. Package.ruleDefRule

    go [] = case rule ^. ruleHead . ruleValue of
        Nothing   -> return $ BoolV True
        Just term -> evalTerm term

    go (lit : lits)
        | lit ^. literalNegation = do
            negation trueish $ evalExpr $ lit ^. literalExpr
            go lits
        | otherwise = do
            r <- evalExpr $ lit ^. literalExpr
            if trueish r then go lits else cut

    trueish (BoolV False) = False
    trueish _             = True

evalScalar :: Scalar a -> EvalM Value
evalScalar (String t) = return $ StringV t
evalScalar (Number t) = return $ NumberV t
evalScalar (Bool   b) = return $ BoolV   b
evalScalar Null       = return $ NullV

evalObjectKey :: ObjectKey a -> EvalM Value
evalObjectKey (ScalarK _ s) = evalScalar s

evalBinOp :: Expr a -> BinOp -> Expr a -> EvalM Value
evalBinOp x op y = do
    xv <- evalExpr x
    yv <- evalExpr y
    case (xv, op, yv) of
        (_, AssignO, _)   -> return $! BoolV $! xv == yv
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
            "evalBinOp: invalid arguments for " ++ show (PP.pretty' op) ++
            ": " ++ describeValue xv ++ ", " ++ describeValue yv

unify :: Value -> Value -> EvalM ()
unify WildcardV _     = return ()
unify _ WildcardV     = return ()
unify (FreeV alpha) v = unsafeBind alpha v
unify v (FreeV alpha) = unsafeBind alpha v
unify lhs rhs         | lhs == rhs = return ()
unify _ _             = cut
