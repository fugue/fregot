{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Prepare.Order.Tests
    ( tests
    ) where

import qualified Data.HashSet         as HS
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Dsl
import           Fregot.Prepare.Order
import qualified Test.Tasty           as Tasty
import           Test.Tasty.HUnit     ((@?=))
import qualified Test.Tasty.HUnit     as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Fregot.Prepare.Order.Tests"
    [ tests_reorder
    , tests_orderForClosures
    ]

tests_reorder :: Tasty.TestTree
tests_reorder = Tasty.testGroup "reorder"
    [ Tasty.testCase "01" $ reorder needAllSmallerNumbers
        [1, 2, 3, 4] @?= ([1, 2, 3, 4], [])
    , Tasty.testCase "02" $ reorder needAllSmallerNumbers
        [1, 4, 3, 2] @?= ([1, 2, 3, 4], [])
    , Tasty.testCase "03" $ reorder needAllSmallerNumbers
        [1, 5, 3, 2] @?= ([1, 2, 3], [[4]])
    ]
  where
    needAllSmallerNumbers :: [Int] -> Int -> OrderPredicate (HS.HashSet Int)
    needAllSmallerNumbers xs x =
        let missing = HS.fromList [1 .. x - 1] `HS.difference` HS.fromList xs in
        if null missing then OrderOk else OrderError missing

tests_orderForClosures :: Tasty.TestTree
tests_orderForClosures = Tasty.testGroup "orderForClosures"
    [ Tasty.testCase "01" $
        let program =
                [ lit $ TermS $
                    ArrayCompT 0 (var "b") [lit $ UnifyS 0 (var "a") (var "b")]
                , lit $ TermS $
                    ArrayCompT 0 (var "c") [lit $ UnifyS 0 (var "a") (var "c")]
                , lit $ UnifyS 0 (var "a") (num 1)
                ] in
        testOrderForClosures program @?=
        [program !! 2, program !! 0, program !! 1]
    ]
  where
    testOrderForClosures = fst . orderForClosures (const 2) mempty
