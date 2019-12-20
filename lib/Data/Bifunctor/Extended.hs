module Data.Bifunctor.Extended
    ( module Data.Bifunctor
    , both
    ) where

import           Data.Bifunctor

both :: Bifunctor f => (a -> b) -> f a a -> f b b
both f = bimap f f
