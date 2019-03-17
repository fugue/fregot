{-# LANGUAGE Rank2Types #-}
module Data.HashMap.Strict.Extended
    ( module Data.HashMap.Strict
    , shortcuts
    ) where

import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict
import           Prelude             hiding (lookup)

shortcuts :: (Eq k, Hashable k) => HashMap k k -> HashMap k a -> HashMap k a
shortcuts cuts base = foldlWithKey'
    (\acc from to -> case lookup to acc of
        Just v | Nothing <- lookup from acc -> insert from v acc
        _                                   -> acc)
    base
    cuts
