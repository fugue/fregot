module Main where

import qualified Data.Kleene.Tests
import qualified Data.SafeVar.Tests
import qualified Fregot.Compile.Order.Tests
import qualified Fregot.Interpreter.Dependencies.Tests
import qualified Fregot.Interpreter.Tests
import qualified Fregot.Names.Tests
import qualified Fregot.Parser.Tests
import qualified Fregot.Prepare.Lens.Tests
import qualified Fregot.Tree.Tests
import qualified Fregot.Types.Internal.Tests
import qualified System.Console.Haskeline.Tests
import qualified Test.Tasty                            as Tasty

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "fregot"
    [ Data.Kleene.Tests.tests
    , Data.SafeVar.Tests.tests
    , Fregot.Compile.Order.Tests.tests
    , Fregot.Interpreter.Dependencies.Tests.tests
    , Fregot.Interpreter.Tests.tests
    , Fregot.Names.Tests.tests
    , Fregot.Parser.Tests.tests
    , Fregot.Prepare.Lens.Tests.tests
    , Fregot.Tree.Tests.tests
    , Fregot.Types.Internal.Tests.tests
    , System.Console.Haskeline.Tests.tests
    ]
