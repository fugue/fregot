module Data.List.NonEmpty.Extended
    ( module Data.List.NonEmpty
    , singleton
    , fromList
    , catMaybes
    ) where

import           Data.List.NonEmpty hiding (fromList)
import qualified Data.Maybe as Maybe

singleton :: a -> NonEmpty a
singleton x = x :| []

fromList :: [a] -> Maybe (NonEmpty a)
fromList []       = Nothing
fromList (x : xs) = Just (x :| xs)

catMaybes :: NonEmpty (Maybe a) -> Maybe (NonEmpty a)
catMaybes = fromList . Maybe.catMaybes . toList
