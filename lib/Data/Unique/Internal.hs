{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Unique.Internal
    ( Unique (..)
    , HasUnique (..)
    , getUniqueInt

    , GlobalUniqueGen
    , newGlobalUniqueGen
    , getGlobalUnique

    , StableUniqueGen
    , newStableUniqueGen
    , getStableUnique
    , getFreshUnique
    ) where

import qualified Data.Aeson               as Aeson
import           Data.Data                (Data)
import           Data.Hashable            (Hashable (..))
import qualified Data.HashMap.Strict      as HMS
import           Data.IORef               (IORef, atomicModifyIORef', newIORef)
import           GHC.Generics             (Generic)
import           System.IO.Unsafe         (unsafePerformIO)

newtype Unique = Unique {unUnique :: Int}
    deriving (Aeson.ToJSON, Eq, Ord, Enum, Data, Num, Generic)

instance Show Unique where
    show (Unique x) = show x

instance Hashable Unique where
    hashWithSalt salt (Unique a) = hashWithSalt salt a
    {-# INLINE hashWithSalt #-}

class HasUnique k where
    getUnique :: k -> Unique

getUniqueInt :: HasUnique k => k -> Int
getUniqueInt = unUnique . getUnique
{-# INLINE getUniqueInt #-}

type GlobalUniqueGen = IORef Unique

newGlobalUniqueGen :: IO GlobalUniqueGen
newGlobalUniqueGen = newIORef 0

getGlobalUnique :: GlobalUniqueGen -> (Unique -> a) -> a
getGlobalUnique ref f = unsafePerformIO $ atomicModifyIORef' ref $ \st ->
    (st + 1, f st)
{-# NOINLINE getGlobalUnique #-}

type StableUniqueGen a = IORef (UniqueGenSt a)

data UniqueGenSt a = UniqueGenSt
  { unique      :: !Unique
  , uniqueTable :: !(HMS.HashMap a Unique)
  }

newStableUniqueGen :: (Eq a, Hashable a) => IO (StableUniqueGen a)
newStableUniqueGen = newIORef (UniqueGenSt 0 HMS.empty)

getStableUnique
    :: (Eq a, Hashable a) => StableUniqueGen a -> a -> Unique
getStableUnique ref k = unsafePerformIO $ atomicModifyIORef' ref $ \st ->
    let tbl = uniqueTable st in
    case HMS.lookup k tbl of
      Just i  -> (st, i)
      Nothing ->
        let !i = unique st in
        (st{ unique = succ i, uniqueTable = HMS.insert k i tbl }, i)
{-# NOINLINE getStableUnique #-}

-- | Get a fresh ID without the value being stable and stored in the hash map.
getFreshUnique :: StableUniqueGen a -> (Unique -> b) -> b
getFreshUnique ref f = unsafePerformIO $ atomicModifyIORef' ref $ \st ->
    let fresh = unique st in
    (st {unique = succ fresh}, f fresh)
{-# NOINLINE getFreshUnique #-}
