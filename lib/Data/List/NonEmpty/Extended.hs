{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE CPP #-}
module Data.List.NonEmpty.Extended
    ( module Data.List.NonEmpty
    , singleton
    , fromList
    , catMaybes
    , (++:)
    ) where

#if MIN_VERSION_base(4, 15, 0)
import           Data.List.NonEmpty hiding (fromList, singleton)
#else
import           Data.List.NonEmpty hiding (fromList)
#endif
import qualified Data.Maybe as Maybe

singleton :: a -> NonEmpty a
singleton x = x :| []

fromList :: [a] -> Maybe (NonEmpty a)
fromList []       = Nothing
fromList (x : xs) = Just (x :| xs)

catMaybes :: NonEmpty (Maybe a) -> Maybe (NonEmpty a)
catMaybes = fromList . Maybe.catMaybes . toList

(++:) :: NonEmpty a -> [a] -> NonEmpty a
(++:) (x :| xs) ys = x :| (xs ++ ys)
