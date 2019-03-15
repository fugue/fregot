{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Prepare.Vars.Tests where

import           Fregot.Prepare.Ast
import           Fregot.Prepare.Vars
import qualified Test.Tasty          as Tasty
import           Test.Tasty.HUnit    ((@?=))
import qualified Test.Tasty.HUnit    as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Fregot.Prepare.Vars.Tests"
    [ Tasty.testCase "ovRuleBody 01" $
        ovRuleBody (const 2) mempty
            [ literal $ TermS $ RefT 0 (var "a") (var "b")
            ] @?=
        Safe ["b"]
    , Tasty.testCase "ovRuleBody 02" $
        ovRuleBody (const 2) (Safe ["a"])
            [ literal $ UnifyS 0 (var "a") (var "b")
            ] @?=
        Safe ["b"]
    , Tasty.testCase "ovRuleBody 03" $
        ovRuleBody (const 2) (Safe ["b"])
            [ literal $ UnifyS 0 (var "a") (var "b")
            ] @?=
        Safe ["a"]
    , Tasty.testCase "ovRuleBody 04" $
        ovRuleBody (const 2) mempty
            [ literal $ UnifyS 0 (var "a") (num 1)
            , literal $ UnifyS 0 (var "b") (var "a")
            ] @?=
        Safe ["a", "b"]
    , Tasty.testCase "ovRuleBody 05" $
        ovRuleBody (const 2) mempty
            [ literal $ TermS $
                CallT 0 (NamedFunction ["add"]) [var "a", var "b", var "x"]
            ] @?=
        Safe ["x"]
    , Tasty.testCase "ovRuleBody 06" $
        ovRuleBody (const 2) mempty
            [ literal $ UnifyS 0
                (var "x")
                (CallT 0 (NamedFunction ["add"]) [var "a", var "b"])
            ] @?=
        Safe ["x"]
    ]

var :: Var -> Term Int
var = VarT 0

num :: Int -> Term Int
num = ScalarT 0 . Number . fromIntegral
