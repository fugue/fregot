module Fregot.Interpreter.Dependencies.Tests
    ( tests
    ) where

import           Control.Lens                    ((&), (.~), (^?), _Right)
import qualified Data.HashSet.Extended           as HS
import           Fregot.Interpreter.Dependencies as Deps
import qualified Test.Tasty                      as Tasty
import           Test.Tasty.HUnit                ((@?=))
import qualified Test.Tasty.HUnit                as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Fregot.Interpreter.Dependencies.Tests"
    [ Tasty.testCase "plan" $
        let deps x = [d | d <- [0 .. x - 1], odd x == odd d]
            graph  = Deps.empty
                & Deps.graphDone .~ (`elem` [2, 3])
                & Deps.graphDependencies .~ deps in

        Deps.plan graph (HS.fromList [8]) ^? _Right @?= Just [0, 4, 6, 8 :: Int]
    ]
