{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Unique
    ( Unique (..)
    , HasUnique (..)

    , Uniquely (..)

    , GlobalUniqueGen
    , newGlobalUniqueGen
    , getGlobalUnique

    , StableUniqueGen
    , newStableUniqueGen
    , getStableUnique
    , getFreshUnique
    ) where

import qualified Data.Aeson          as Aeson
import           Data.Hashable       (Hashable (..))
import qualified Data.HashMap.Strict as HMS
import           Data.IORef          (IORef)
import qualified Data.IORef          as IORef
import           System.IO.Unsafe    (unsafePerformIO)

newtype Unique = Unique {unUnique :: Int}
    deriving (Aeson.ToJSON, Eq, Ord, Enum, Num)

instance Show Unique where
    show (Unique x) = show x

instance Hashable Unique where
    hashWithSalt salt (Unique a) = hashWithSalt salt a
    {-# INLINE hashWithSalt #-}

class HasUnique k where
    getUnique :: k -> Unique

newtype Uniquely k = Uniquely {unUniquely :: k}

instance HasUnique k => Eq (Uniquely k) where
    Uniquely x == Uniquely y = getUnique x == getUnique y
    {-# INLINE (==) #-}

instance HasUnique k => Ord (Uniquely k) where
    compare (Uniquely x) (Uniquely y) = compare (getUnique x) (getUnique y)
    {-# INLINE compare #-}

instance HasUnique k => Hashable (Uniquely k) where
    hashWithSalt salt = hashWithSalt salt . getUnique . unUniquely
    {-# INLINE hashWithSalt #-}

type GlobalUniqueGen = IORef Unique

newGlobalUniqueGen :: IO GlobalUniqueGen
newGlobalUniqueGen = IORef.newIORef 0

getGlobalUnique :: GlobalUniqueGen -> (Unique -> a) -> a
getGlobalUnique ref f = unsafePerformIO $ IORef.atomicModifyIORef' ref $ \st ->
    (st + 1, f st)
{-# NOINLINE getGlobalUnique #-}

type StableUniqueGen a = IORef (UniqueGenSt a)

data UniqueGenSt a = UniqueGenSt
  { unique      :: !Unique
  , uniqueTable :: !(HMS.HashMap a Unique)
  }

newStableUniqueGen :: (Eq a, Hashable a) => IO (StableUniqueGen a)
newStableUniqueGen = IORef.newIORef (UniqueGenSt 0 HMS.empty)

getStableUnique
    :: (Eq a, Hashable a) => StableUniqueGen a -> a -> Unique
getStableUnique ref k = unsafePerformIO $ IORef.atomicModifyIORef' ref $ \st ->
    let tbl = uniqueTable st in
    case HMS.lookup k tbl of
      Just i  -> (st, i)
      Nothing ->
        let !i = unique st in
        (st{ unique = succ i, uniqueTable = HMS.insert k i tbl }, i)
{-# NOINLINE getStableUnique #-}

-- | Get a fresh ID without the value being stable and stored in the hash map.
getFreshUnique :: StableUniqueGen a -> (Unique -> b) -> b
getFreshUnique ref f = unsafePerformIO $ IORef.atomicModifyIORef' ref $ \st ->
    let fresh = unique st in
    (st {unique = succ fresh}, f fresh)
{-# NOINLINE getFreshUnique #-}
