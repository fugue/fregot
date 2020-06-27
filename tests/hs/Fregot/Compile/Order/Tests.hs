{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

-- These instances are only used in this file.
deriving instance Eq a => Eq (Literal a)
deriving instance Eq a => Eq (Statement a)
deriving instance Eq a => Eq (Comprehension a)
deriving instance Eq a => Eq (IndexedComprehension a)
deriving instance Eq a => Eq (Term a)
deriving instance Eq a => Eq (With a)
deriving instance Eq WithPath

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
                [ lit $ TermS $ CompT source $
                    ArrayComp (name "b") [lit $ UnifyS source (name "a") (name "b")]
                , lit $ TermS $ CompT source $
                    ArrayComp (name "c") [lit $ UnifyS source (name "a") (name "c")]
                , lit $ UnifyS source (name "a") (num 1)
                ] in
        testOrderForClosures program @?=
        [program !! 2, program !! 0, program !! 1]
    ]
  where
    testOrderForClosures = fst . orderForClosures inferEnv mempty

tests_orderForSafety :: Tasty.TestTree
tests_orderForSafety = Tasty.testGroup "orderForSafety"
    [ Tasty.testCase "01" $
        let program =
                [ lit $ UnifyS source (name "a") (num 1)
                , lit $ UnifyS source
                    (name "x")
                    (call (BuiltinName "add") [name "a", name "b", name "x"])
                , lit $ UnifyS source (name "b") (num 2)
                ] in
        testOrderForSafety program @?=
        [program !! 0, program !! 2, program !! 1]
    , Tasty.testCase "02" $ testOrderForSafety
        [ lit $ TermS $ CompT source $
            ArrayComp (name "c")
                [ lit $ UnifyS source (name "c") (name "b")
                , lit $ UnifyS source (name "b") (name "a")
                ]
        , lit $ UnifyS source (name "a") (num 1)
        ] @?=
        [ lit $ UnifyS source (name "a") (num 1)
        , lit $ TermS $ CompT source $
            ArrayComp (name "c")
                [ lit $ UnifyS source (name "b") (name "a")
                , lit $ UnifyS source (name "c") (name "b")
                ]
        ]
    ]
  where
    testOrderForSafety = fst . orderForSafety inferEnv mempty
