-- | Horrible DSL to be able to quickly construct syntax for use in tests.
module Fregot.Prepare.Dsl where

import           Fregot.Prepare.Ast

lit :: Statement Int -> Literal Int
lit = literal 0

var :: Var -> Term Int
var = VarT 0

num :: Int -> Term Int
num = ScalarT 0 . Number . fromIntegral

call :: Var -> [Term Int] -> Term Int
call v args = CallT 0 (NamedFunction [v]) args
