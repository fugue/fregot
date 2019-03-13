module Data.SafeVar.Tests where

import           Control.Lens ((^.))
import qualified Data.HashSet as HS
import qualified Data.Map     as Map
import qualified Data.SafeVar as SafeVar
import qualified Language.Dot as Dot
import qualified Test.Tasty   as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Data.SafeVar.Tests" []

type Var = String

data SimpleExpr
    = LitE Int
    | VarE Var
    | AddE SimpleExpr SimpleExpr

instance Show SimpleExpr where
    show (LitE i)   = show i
    show (VarE v)   = v
    show (AddE x y) = show x ++ " + " ++ show y

data SimpleStatement
    = UnifyS      SimpleExpr SimpleExpr
    | AssignS      Var       SimpleExpr
    | GreaterThanS SimpleExpr SimpleExpr

instance Show SimpleStatement where
    show (UnifyS       x y) = show x ++ " = "  ++ show y
    show (AssignS      v x) = v      ++ " := " ++ show x
    show (GreaterThanS x y) = show x ++ " > "  ++ show y

type Program = [SimpleStatement]

program01 :: Program
program01 =
    [ GreaterThanS (VarE "x") (VarE "y")
    , UnifyS       (VarE "x") (LitE 1)
    , UnifyS       (LitE 2)   (VarE "y")
    ]

program02 :: Program
program02 =
    [ GreaterThanS (AddE (VarE "z") (VarE "x")) (VarE "y")
    , UnifyS       (VarE "z") (LitE 1)
    , UnifyS       (VarE "x") (VarE "y")
    , UnifyS       (VarE "z") (VarE "x")
    ]

freeVars :: SimpleExpr -> HS.HashSet Var
freeVars (LitE _)   = HS.empty
freeVars (VarE x)   = HS.singleton x
freeVars (AddE x y) = freeVars x <> freeVars y

toStatement :: SimpleStatement -> SafeVar.Statement SimpleStatement Var
toStatement ss = SafeVar.Statement ss inVars outVars
  where
    (inVars, outVars) = case ss of
        GreaterThanS x y -> (freeVars x <> freeVars y, mempty)
        AssignS v x      -> (freeVars x, HS.singleton v)
        UnifyS x y       ->
            let xf = freeVars x
                yf = freeVars y in
            case (HS.null xf, HS.null yf) of
                (True,  True)  -> (mempty, mempty)
                (False, True)  -> (mempty, xf)
                (True,  False) -> (mempty, yf)
                (False, False) -> let f = xf <> yf in (f, f)

programToDot :: Program -> Dot.Dot
programToDot =
    Dot.digraph showNode show .
    (map (\(k, (v, n)) -> (v, k, n)) . Map.toList) .
    SafeVar.toGraph .  fmap toStatement
  where
    showNode (SafeVar.VarNode       x) = "[" ++ x ++ "]"
    showNode (SafeVar.StatementNode s) = show (s ^. SafeVar.statementExpr)
