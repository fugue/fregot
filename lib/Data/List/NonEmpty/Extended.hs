module Data.List.NonEmpty.Extended
    ( module Data.List.NonEmpty
    , singleton
    ) where

import           Data.List.NonEmpty

singleton :: a -> NonEmpty a
singleton x = x :| []
