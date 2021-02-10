{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Vector.Extended
    ( module Data.Vector
    , lookup
    , inits
    ) where

import           Data.Vector
import qualified Data.Vector           as V
import           Data.Vector.Instances ()
import           Prelude               hiding (head, length, lookup, null, tail)

lookup :: Eq k => k -> Vector (k, v) -> Maybe v
lookup k = fmap snd . find ((== k) . fst)

inits :: Vector k -> [Vector k]
inits vec = [V.take n vec | n <- [0 .. length vec - 1]]
