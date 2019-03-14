module Main where

import qualified Data.SafeVar.Tests
import qualified Fregot.Prepare.Vars.Tests
import qualified Test.Tasty                as Tasty

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "fregot"
    [ Data.SafeVar.Tests.tests
    , Fregot.Prepare.Vars.Tests.tests
    ]
