{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Prepare.Vars.Tests where

import           Control.Lens                ((&), (.~))
import qualified Data.HashMap.Strict         as HMS
import           Data.List.NonEmpty.Extended (NonEmpty)
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Vars
import qualified Test.Tasty                  as Tasty
import           Test.Tasty.HUnit            ((@?=))
import qualified Test.Tasty.HUnit            as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Fregot.Prepare.Vars.Tests"
    [ Tasty.testCase "freeRuleBody 01" $
        freeRuleBody emptyScope
            [ literal $ UnifyS 0 (VarT 0 "a") (VarT 0 "b")
            ] @?=
        mkVars [("a", [0 :: Int]) ,("b", [0])] []
    , Tasty.testCase "freeRuleBody 02" $
        freeRuleBody emptyScope
            [ literal $ UnifyS 0 (VarT 0 "b") (ScalarT 0 (String "wat"))
            ] @?=
        mkVars [] ["b"]
        {-
    , Tasty.testCase "freeRuleBody 03" $
        freeRuleBody emptyScope
            [ literal $ UnifyS 0 (VarT 0 "a") (ScalarT 0 (String "wat"))
            , literal $ UnifyS 0 (VarT 0 "b") (VarT 0 "a")
            ] @?=
        mkVars [] ["b"]
        -}
    ]
  where
    mkVars :: [(Var, NonEmpty Int)] -> [Var] -> Vars Var Int
    mkVars ins outs =
        mempty
            & varsIn .~ HMS.fromList ins
            & varsOut .~ HMS.fromList (zip outs (repeat ()))
