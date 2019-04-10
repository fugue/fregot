{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Data.Unique.Map
  ( Unique
  , HasUnique (..)

  , UniqueMap
  , delete
  , elems
  , empty
  , filter
  , fromList
  , insert
  , lookup
  , member
  , singleton
  , null
  , size
  , toList
  , keys
  , union
  ) where

import           Data.Bifunctor       (first)
import           Data.Hashable        (Hashable (..))
import qualified Data.HashMap.Strict  as HMS
import           Data.Semigroup       (Semigroup)
import           Data.Unique.Internal
import           Prelude              hiding (filter, lookup, map, null)

newtype UniqueKey k = UniqueKey {unUniqueKey :: k} deriving (Show)

instance HasUnique k => Eq (UniqueKey k) where
    UniqueKey x == UniqueKey y = getUnique x == getUnique y
    {-# INLINE (==) #-}

instance HasUnique k => Hashable (UniqueKey k) where
    hashWithSalt salt (UniqueKey k) = hashWithSalt salt (getUnique k)
    {-# INLINE hashWithSalt #-}

newtype UniqueMap k v = UniqueMap
    { unUniqueMap :: HMS.HashMap (UniqueKey k) v
    } deriving (Show, Foldable, Monoid, Semigroup, Traversable, Functor)

empty :: UniqueMap k v
empty = UniqueMap HMS.empty

singleton :: HasUnique k => k -> v -> UniqueMap k v
singleton k = UniqueMap . HMS.singleton (UniqueKey k)
{-# INLINE singleton #-}

lookup :: HasUnique k => k -> UniqueMap k v -> Maybe v
lookup k = HMS.lookup (UniqueKey k) . unUniqueMap
{-# INLINE lookup #-}

fromList :: HasUnique k => [(k, v)] -> UniqueMap k v
fromList kvs = UniqueMap $ HMS.fromList [(UniqueKey k, v) | (k,v) <- kvs]
{-# INLINE fromList #-}

elems :: UniqueMap k v -> [v]
elems = HMS.elems . unUniqueMap
{-# INLINE elems #-}

toList :: HasUnique k => UniqueMap k v -> [(k, v)]
toList = fmap (first unUniqueKey) . HMS.toList . unUniqueMap
{-# INLINE toList #-}

keys :: HasUnique k => UniqueMap k v -> [k]
keys = fmap fst . toList
{-# INLINE keys #-}

union :: HasUnique k => UniqueMap k v -> UniqueMap k v -> UniqueMap k v
union (UniqueMap x) (UniqueMap y) = UniqueMap (HMS.union x y)
{-# INLINE union #-}

delete :: HasUnique k => k -> UniqueMap k v -> UniqueMap k v
delete k = UniqueMap . HMS.delete (UniqueKey k) . unUniqueMap
{-# INLINE delete #-}

insert :: HasUnique k => k -> v -> UniqueMap k v -> UniqueMap k v
insert k v = UniqueMap . HMS.insert (UniqueKey k) v . unUniqueMap
{-# INLINE insert #-}

filter :: (v -> Bool) -> UniqueMap k v -> UniqueMap k v
filter f = UniqueMap . HMS.filter f . unUniqueMap
{-# INLINE filter #-}

member :: HasUnique k => k -> UniqueMap k a -> Bool
member k = HMS.member (UniqueKey k) . unUniqueMap
{-# INLINE member #-}

null :: UniqueMap k v -> Bool
null = HMS.null . unUniqueMap
{-# INLINE null #-}

size :: UniqueMap k v -> Int
size = HMS.size . unUniqueMap
{-# INLINE size #-}
