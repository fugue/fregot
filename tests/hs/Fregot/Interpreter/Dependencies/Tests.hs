{-# LANGUAGE OverloadedLists #-}
module Fregot.Interpreter.Dependencies.Tests
    ( tests
    ) where

import           Control.Lens                    ((^?), _Right)
import qualified Data.HashMap.Strict             as HMS
import           Fregot.Interpreter.Dependencies as Deps
import qualified Test.Tasty                      as Tasty
import           Test.Tasty.HUnit                ((@?=))
import qualified Test.Tasty.HUnit                as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Fregot.Interpreter.Dependencies.Tests"
    [ Tasty.testCase "plan" $
        let graph = Graph [(2, ()), (3, ())] dependOnLower in
        Deps.plan graph [8] ^? _Right @?= Just [0, 4, 6, 8 :: Int]
    , Tasty.testCase "evict" $
        let graph = Graph
                (HMS.fromList $ zip [0 .. 10] $ repeat ())
                dependOnLower in
        Deps.evict graph [5] @?= [5, 7, 9 :: Int]
    ]
  where
    dependOnLower x = [d | d <- [0 .. x - 1], odd x == odd d]
