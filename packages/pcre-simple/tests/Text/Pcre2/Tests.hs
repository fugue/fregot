{-# LANGUAGE OverloadedStrings #-}
module Text.Pcre2.Tests
    ( tests
    ) where

import qualified Test.Tasty       as Tasty
import           Test.Tasty.HUnit ((@?=))
import qualified Test.Tasty.HUnit as Tasty
import           Text.Pcre2

tests :: Tasty.TestTree
tests = Tasty.testGroup "Ludwig.Pcre2.Tests"
    -- This test case shouldn't generate empty matches.
    [ Tasty.testCase "match_test01" $
        (do
            regex <- compile ".?"
            match regex "blah") @?=
        Right ([Match (Range i 1) [] | i <- [0 .. 3]] ++ [Match (Range 4 0) []])

    -- This one, on the other hand, should generate a bunch of them.
    , Tasty.testCase "match_test02" $
        (do
            regex <- compile "\\d*"
            match regex "o1o2") @?=
        Right
            [ Match (Range 0 0) []
            , Match (Range 1 1) []
            , Match (Range 2 0) []
            , Match (Range 3 1) []
            , Match (Range 4 0) []
            ]

    -- Matching multiple lines.
    , Tasty.testCase "match_test03" $
        (do
            regex <- compileWith multilineOpts "/lw$"
            match regex "foo/lw\ntest/lib\nbar/lw\n") @?=
        Right
            [ Match (Range  3 3) []
            , Match (Range 19 3) []
            ]

    -- Matching empty text.
    , Tasty.testCase "match_test04" $
        (do
            regex <- compile "[a-z]*"
            match regex "") @?=
        Right [Match (Range  0 0) []]
    ]
  where
    multilineOpts = defaultCompileOptions {coMultiline = True}
