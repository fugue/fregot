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
