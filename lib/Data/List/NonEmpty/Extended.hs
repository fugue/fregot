module Data.List.NonEmpty.Extended
    ( module Data.List.NonEmpty
    , singleton
    , fromList
    ) where

import           Data.List.NonEmpty hiding (fromList)

singleton :: a -> NonEmpty a
singleton x = x :| []

fromList :: [a] -> Maybe (NonEmpty a)
fromList []       = Nothing
fromList (x : xs) = Just (x :| xs)
