{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
module Data.Bifunctor.Extended
    ( module Data.Bifunctor
    , both
    ) where

import           Data.Bifunctor

both :: Bifunctor f => (a -> b) -> f a a -> f b b
both f = bimap f f
