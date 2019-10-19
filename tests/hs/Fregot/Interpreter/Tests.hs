{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Interpreter.Tests
    ( tests
    ) where

import           Control.Lens              (review, view)
import           Control.Monad.Identity    (Identity)
import           Control.Monad.Parachute   (runParachuteT)
import qualified Fregot.Eval               as Eval
import qualified Fregot.Eval.Builtins      as B
import qualified Fregot.Eval.Number        as Number
import qualified Fregot.Eval.Value         as Eval
import qualified Fregot.Interpreter        as Interpreter
import           Fregot.Names              (Name (..))
import qualified Fregot.Repl.Parse         as Repl
import qualified Fregot.Sources            as Sources
import qualified Fregot.TypeCheck.Builtins as Types
import qualified Fregot.TypeCheck.Types    as Types
import qualified Test.Tasty                as Tasty
import           Test.Tasty.HUnit          ((@?=))
import qualified Test.Tasty.HUnit          as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Fregot.Interpreter.Tests"
    [ Tasty.testCase "insertBuiltin" $ do
        sources          <- Sources.newHandle
        interpreter      <- Interpreter.newHandle sources
        (errs, mbResult) <- runParachuteT $ do
            Interpreter.insertBuiltin interpreter magicName magicImpl
            let input = "test.magic()"
            query <- Repl.parseRuleOrQuery Sources.TestInput input >>=
                either (const $ error "expected expression") return
            Interpreter.evalQuery interpreter Nothing "testy" query

        Tasty.assertBool "No errors" $ null errs
        map (view Eval.rowValue) <$> mbResult @?=
            Just [Eval.NumberV $ review Number.int 101]
    ]
  where
    magicName :: B.Function
    magicName = B.NamedFunction (QualifiedName "test" "magic")

    magicImpl :: B.Builtin Identity
    magicImpl = B.Builtin
        B.Out
        (Types.out Types.Number) $
        pure $ \B.Nil -> return $! review Number.int 101
