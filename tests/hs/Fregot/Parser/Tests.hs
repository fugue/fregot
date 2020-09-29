{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Parser.Tests
    ( tests
    ) where

import           Control.Lens            (view, (&), (.~))
import           Control.Monad.Identity  (runIdentity)
import           Control.Monad.Parachute (runParachuteT)
import qualified Fregot.Parser           as Parser
import qualified Fregot.Sources          as Sources
import qualified Fregot.Sugar            as Sugar
import qualified Test.Tasty              as Tasty
import           Test.Tasty.HUnit        ((@?=))
import qualified Test.Tasty.HUnit        as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Fregot.Parser.Tests"
    [ test_parseWithDefaultPackageName
    ]

test_parseWithDefaultPackageName :: Tasty.TestTree
test_parseWithDefaultPackageName =
    Tasty.testGroup "parse with defaultPackageName"
        [ Tasty.testCase "present in module" $
            view Sugar.modulePackage <$> lexAndParse "package foo\na {true}"
                @?= Just "foo"
        , Tasty.testCase "not present in module" $
            view Sugar.modulePackage <$> lexAndParse "b {false}"
                @?= Just "policy"
        ]
  where
    parserOpts = Parser.defaultParserOptions
        & Parser.poDefaultPackageName .~ Just "policy"
    lexAndParse = snd . runIdentity . runParachuteT .
        Parser.lexAndParse (Parser.parseModule parserOpts) Sources.TestInput
