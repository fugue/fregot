-- | This set structure does not actually store the values of the things inside
-- it.  As such, it supports membership tests, but it cannot be converted back
-- to a list.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Unique.Set
    ( Unique
    , HasUnique (..)

    , Set

    , empty
    , singleton
    , union
    , difference
    , insert
    , fromList

    , null
    , member
    ) where

import qualified Data.HashSet.Extended as HS
import           Data.Unique.Internal
import           Prelude               hiding (null)

newtype Set a = Set {unSet :: HS.HashSet Int}

empty :: HasUnique a => Set a
empty = Set HS.empty
{-# INLINE empty #-}

singleton :: HasUnique a => a -> Set a
singleton = Set . HS.singleton . getUniqueInt
{-# INLINE singleton #-}

union :: HasUnique a => Set a -> Set a -> Set a
union (Set x) (Set y) = Set (HS.union x y)
{-# INLINE union #-}

difference :: HasUnique a => Set a -> Set a -> Set a
difference (Set x) (Set y) = Set (HS.difference x y)
{-# INLINE difference #-}

insert :: HasUnique a => a -> Set a -> Set a
insert x = Set . HS.insert (getUniqueInt x) . unSet
{-# INLINE insert #-}

fromList :: HasUnique a => [a] -> Set a
fromList = Set . HS.fromList . map getUniqueInt
{-# INLINE fromList #-}

null :: HasUnique a => Set a -> Bool
null = HS.null . unSet
{-# INLINE null #-}

member :: HasUnique a => a -> Set a -> Bool
member x = HS.member (getUniqueInt x) . unSet
{-# INLINE member #-}
