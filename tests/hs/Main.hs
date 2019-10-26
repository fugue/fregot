module Main where

import qualified Data.SafeVar.Tests
import qualified Fregot.Compile.Order.Tests
import qualified Fregot.Interpreter.Dependencies.Tests
import qualified Fregot.Interpreter.Tests
import qualified Fregot.Names.Tests
import qualified Fregot.Parser.Tests
import qualified Fregot.Prepare.Lens.Tests
import qualified Fregot.Prepare.Vars.Tests
import qualified Fregot.TypeCheck.Types.Tests
import qualified System.Console.Haskeline.Tests
import qualified Test.Tasty                            as Tasty

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "fregot"
    [ Data.SafeVar.Tests.tests
    , Fregot.Compile.Order.Tests.tests
    , Fregot.Interpreter.Dependencies.Tests.tests
    , Fregot.Interpreter.Tests.tests
    , Fregot.Names.Tests.tests
    , Fregot.Parser.Tests.tests
    , Fregot.Prepare.Lens.Tests.tests
    , Fregot.Prepare.Vars.Tests.tests
    , Fregot.TypeCheck.Types.Tests.tests
    , System.Console.Haskeline.Tests.tests
    ]
