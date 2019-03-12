module Main where

import qualified Test.Tasty as Tasty
import qualified Data.SafeVar.Tests

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "fregot"
    [ Data.SafeVar.Tests.tests
    ]
