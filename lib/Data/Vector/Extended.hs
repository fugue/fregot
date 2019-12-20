{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Vector.Extended
    ( module Data.Vector
    , uncons
    , catMaybes
    , lookup
    ) where

import           Data.Vector
import           Data.Vector.Instances ()
import qualified Data.Vector.Mutable   as VM
import           Prelude               hiding (head, length, lookup, null, tail)

uncons :: Vector a -> Maybe (a, Vector a)
uncons vec = if null vec then Nothing else Just (head vec, tail vec)

catMaybes :: Vector (Maybe a) -> Vector a
catMaybes vec0 = create $ do
    vec1 <- VM.new (length vec0)
    let go !i !j
            | i >= length vec0   = return $ VM.take j vec1
            | Just x <- vec0 ! i = VM.write vec1 j x >> go (i + 1) (j + 1)
            | otherwise          = go (i + 1) j
    go 0 0

lookup :: Eq k => k -> Vector (k, v) -> Maybe v
lookup k = fmap snd . find ((== k) . fst)
