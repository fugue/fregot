{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Compile.Order.Tests
    ( tests
    ) where

import qualified Data.HashSet         as HS
import           Fregot.Compile.Order
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Dsl
import qualified Test.Tasty           as Tasty
import           Test.Tasty.HUnit     ((@?=))
import qualified Test.Tasty.HUnit     as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Fregot.Compile.Order.Tests"
    [ tests_reorder
    , tests_orderForClosures
    , tests_orderForSafety
    ]

tests_reorder :: Tasty.TestTree
tests_reorder = Tasty.testGroup "reorder"
    [ Tasty.testCase "01" $ reorderSmallerNumbers
        [1, 2, 3, 4] @?= ([1, 2, 3, 4], [])
    , Tasty.testCase "02" $ reorderSmallerNumbers
        [1, 4, 3, 2] @?= ([1, 2, 3, 4], [])
    , Tasty.testCase "03" $ reorderSmallerNumbers
        [1, 5, 3, 2] @?= ([1, 2, 3], [[4]])
    ]
  where
    reorderSmallerNumbers xs =
        let (_, ys, es) = reorder needAllSmallerNumbers mempty xs in
        (ys, es)

    needAllSmallerNumbers
        :: HS.HashSet Int -> [Int] -> Int
        -> OrderPredicate (HS.HashSet Int) (HS.HashSet Int)
    needAllSmallerNumbers acc _xs x =
        let missing = HS.fromList [1 .. x - 1] `HS.difference` acc in
        if null missing
            then OrderOk (HS.insert x acc)
            else OrderError missing

tests_orderForClosures :: Tasty.TestTree
tests_orderForClosures = Tasty.testGroup "orderForClosures"
    [ Tasty.testCase "01" $
        let program =
                [ lit $ TermS $
                    ArrayCompT 0 (name "b") [lit $ UnifyS 0 (name "a") (name "b")]
                , lit $ TermS $
                    ArrayCompT 0 (name "c") [lit $ UnifyS 0 (name "a") (name "c")]
                , lit $ UnifyS 0 (name "a") (num 1)
                ] in
        testOrderForClosures program @?=
        [program !! 2, program !! 0, program !! 1]
    ]
  where
    testOrderForClosures = fst . orderForClosures (const $ Just 2) mempty

tests_orderForSafety :: Tasty.TestTree
tests_orderForSafety = Tasty.testGroup "orderForSafety"
    [ Tasty.testCase "01" $
        let program =
                [ lit $ UnifyS 0 (name "a") (num 1)
                , lit $ UnifyS 0
                    (name "x")
                    (call (BuiltinName "add") [name "a", name "b", name "x"])
                , lit $ UnifyS 0 (name "b") (num 2)
                ] in
        testOrderForSafety program @?=
        [program !! 0, program !! 2, program !! 1]
    , Tasty.testCase "02" $ testOrderForSafety
        [ lit $ TermS $
            ArrayCompT 0 (name "c")
                [ lit $ UnifyS 0 (name "c") (name "b")
                , lit $ UnifyS 0 (name "b") (name "a")
                ]
        , lit $ UnifyS 0 (name "a") (num 1)
        ] @?=
        [ lit $ UnifyS 0 (name "a") (num 1)
        , lit $ TermS $
            ArrayCompT 0 (name "c")
                [ lit $ UnifyS 0 (name "b") (name "a")
                , lit $ UnifyS 0 (name "c") (name "b")
                ]
        ]
    ]
  where
    testOrderForSafety = fst . orderForSafety (const $ Just 2) mempty
