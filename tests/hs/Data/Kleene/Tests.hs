{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Kleene.Tests
    ( tests
    ) where

import qualified Data.Kleene         as K
import qualified Test.Tasty.Extended as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Fregot.Kleene.Tests"
    [ Tasty.simpleTestGroup "and" K.and
        [ ([K.False, undefined], K.False)
        , ([K.True, K.Unknown, K.True], K.Unknown)
        , ([], K.True)
        ]
    , Tasty.simpleTestGroup "or" K.or
        [ ([K.False, K.True, undefined], K.True)
        , ([K.False, K.Unknown, K.True], K.True)
        , ([K.False, K.Unknown, K.False], K.Unknown)
        , ([], K.False)
        ]
    ]
