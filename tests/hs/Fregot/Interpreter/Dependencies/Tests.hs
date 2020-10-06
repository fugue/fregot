{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedLists #-}
module Fregot.Interpreter.Dependencies.Tests
    ( tests
    ) where

import           Control.Lens                    ((^?), _Right)
import           Fregot.Interpreter.Dependencies as Deps
import qualified Test.Tasty                      as Tasty
import           Test.Tasty.HUnit                ((@?=))
import qualified Test.Tasty.HUnit                as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Fregot.Interpreter.Dependencies.Tests"
    [ Tasty.testCase "plan" $
        let graph = makeGraph [2, 3] in
        Deps.plan graph [8] ^? _Right @?= Just [0, 4, 6, 8 :: Int]
    , Tasty.testCase "empty plan" $
        let graph = makeGraph [0, 2, 4] in
        Deps.plan graph [4 :: Int] ^? _Right @?= Just []
    , Tasty.testCase "evict" $
        let graph = makeGraph [0 .. 10] in
        Deps.evict graph [5] @?= [5, 7, 9 :: Int]
    ]
  where
    makeGraph l = Graph
        { graphDone         = l
        , graphIsDone       = \x -> x `elem` l
        , graphDependencies = \x -> [d | d <- [0 .. x - 1], odd x == odd d]
        }

