{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import qualified Test.Tasty                as Tasty
import qualified Text.Pcre2.Internal.Tests
import qualified Text.Pcre2.Tests

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "TestSuite"
    [ Text.Pcre2.Internal.Tests.tests
    , Text.Pcre2.Tests.tests
    ]
