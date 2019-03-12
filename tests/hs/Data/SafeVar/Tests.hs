module Data.SafeVar.Tests where

import qualified Data.HashSet as HS
import qualified Data.SafeVar as SafeVar
import qualified Test.Tasty   as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Data.SafeVar.Tests" []

type Var = String

data SimpleExpr
    = LitE Int
    | VarE Var
    | AddE SimpleExpr SimpleExpr
    deriving (Show)

data SimpleStatement
    = UnifyS SimpleExpr SimpleExpr
    | AssignS Var SimpleExpr
    | GreaterThanS SimpleExpr SimpleExpr
    deriving (Show)

program01 :: [SimpleStatement]
program01 =
    [ GreaterThanS (VarE "x") (VarE "y")
    , UnifyS (VarE "x") (LitE 1)
    , UnifyS (LitE 2) (VarE "y")
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
