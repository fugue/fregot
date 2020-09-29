{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE Rank2Types #-}
module Data.HashSet.Extended
    ( module Data.HashSet
    , toHashSetOf
    ) where

import           Control.Lens  (Fold, foldlOf)
import           Data.Hashable (Hashable)
import           Data.HashSet

toHashSetOf :: (Eq a, Hashable a) => Fold s a -> s -> HashSet a
toHashSetOf f = foldlOf f (flip insert) empty
