{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Names.Tests
    ( tests
    ) where

import           Control.Lens        (preview, review)
import           Fregot.Names
import qualified Test.Tasty.Extended as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Fregot.Names.Tests"
    [ Tasty.simpleTestGroup
        "review packageNameFromFilePath"
        (review packageNameFromFilePath)
        [ ("foo.bar", "foo/bar.rego")
        , ("qux",     "qux.rego")
        ]

    , Tasty.simpleTestGroup
        "preview packageNameFromFilePath"
        (preview packageNameFromFilePath)
        [ ("foo/bar.rego", Just "foo.bar")
        , ("qux.rego",     Just "qux")
        , ("p01.rego",     Just "p01")
        , ("p01.txt",      Nothing)
        , ("",             Nothing)
        , ("123a.rego",    Nothing)
        , ("foo._.rego",   Nothing)
        ]
    ]
