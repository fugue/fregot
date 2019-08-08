{-# LANGUAGE Rank2Types #-}
module Data.Traversable.HigherOrder
    ( HTraversable (..)
    ) where

class HTraversable t where
    htraverse :: Applicative f => (forall a. g a -> f (h a)) -> t g -> f (t h)
