{-# LANGUAGE OverloadedLists #-}
module Fregot.Prepare.Order.Tests
    ( tests
    ) where

import qualified Data.HashSet         as HS
import           Fregot.Prepare.Order
import qualified Test.Tasty           as Tasty
import           Test.Tasty.HUnit     ((@?=))
import qualified Test.Tasty.HUnit     as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Fregot.Prepare.Order.Tests"
    [ Tasty.testCase "reorder 01" $ reorder needAllSmallerNumbers
        [1, 2, 3, 4] @?= ([1, 2, 3, 4], [])
    , Tasty.testCase "reorder 02" $ reorder needAllSmallerNumbers
        [1, 4, 3, 2] @?= ([1, 2, 3, 4], [])
    , Tasty.testCase "reorder 03" $ reorder needAllSmallerNumbers
        [1, 5, 3, 2] @?= ([1, 2, 3], [[4]])
    ]
  where
    needAllSmallerNumbers :: [Int] -> Int -> OrderPredicate (HS.HashSet Int)
    needAllSmallerNumbers xs x =
        let missing = HS.fromList [1 .. x - 1] `HS.difference` HS.fromList xs in
        if null missing then OrderOk else OrderError missing
