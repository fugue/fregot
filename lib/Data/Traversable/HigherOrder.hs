{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE Rank2Types #-}
module Data.Traversable.HigherOrder
    ( HTraversable (..)
    ) where

class HTraversable t where
    htraverse :: Applicative f => (forall a. g a -> f (h a)) -> t g -> f (t h)
