{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Prepare.Vars.Tests where

import           Fregot.Prepare.Ast
import           Fregot.Prepare.Dsl
import           Fregot.Prepare.Vars
import qualified Test.Tasty          as Tasty
import           Test.Tasty.HUnit    ((@?=))
import qualified Test.Tasty.HUnit    as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Fregot.Prepare.Vars.Tests"
    [ tests_ovRuleBody
    ]

tests_ovRuleBody :: Tasty.TestTree
tests_ovRuleBody = Tasty.testGroup "ovRuleBody"
    [ Tasty.testCase "01" $ ovRuleBody arities mempty
        [ lit $ TermS $ RefT 0 (name "a") (name "b")
        ] @?=
        Safe ["b"]
    , Tasty.testCase "02" $ ovRuleBody arities (Safe ["a"])
        [ lit $ UnifyS 0 (name "a") (name "b")
        ] @?=
        Safe ["b"]
    , Tasty.testCase "03" $ ovRuleBody arities (Safe ["b"])
        [ lit $ UnifyS 0 (name "a") (name "b")
        ] @?=
        Safe ["a"]
    , Tasty.testCase "04" $ ovRuleBody arities mempty
        [ lit $ UnifyS 0 (name "a") (num 1)
        , lit $ UnifyS 0 (name "b") (name "a")
        ] @?=
        Safe ["a", "b"]
    , Tasty.testCase "05" $ ovRuleBody arities mempty
        [ lit $ TermS $ call "add" [name "a", name "b", name "x"]
        ] @?=
        Safe ["x"]
    , Tasty.testCase "06" $ ovRuleBody arities mempty
        [ lit $ UnifyS 0 (name "x") (call "add" [name "a", name "b"])
        ] @?=
        Safe ["x"]
    ]
  where
    arities = const (Just 2)
