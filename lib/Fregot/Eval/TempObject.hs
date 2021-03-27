{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

A module that allows us to build an object in an interleaved way, and check
for consistencies.  This is used for sets as well as objects.
-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Eval.TempObject
    ( TempObject
    , new
    , Write (..)
    , write
    , read
    , negative
    , isKnownNegative
    ) where

import           Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.Cache          as Cache
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HMS
import qualified Data.IORef          as IORef
import           Prelude             hiding (read)

-- | The known key/values that are part of the object, and a cache of values we
-- know *aren't* in the object.
data Known v = Known !(HMS.HashMap v v) !(Cache.Cache v ())

type TempObject v = IORef.IORef (Known v)

new :: MonadIO m => HMS.HashMap v v -> m (TempObject v)
new known = liftIO . IORef.newIORef . Known known $ Cache.empty (100 * 1024)
{-# INLINE new #-}

data Write v = Ok | Duplicate | Inconsistent v

-- | Write a key/value into the temporary object.  Returns if the write
-- succeeded (i.e. it was not already there).  May raise an exception if
-- there is already a different value for this key.
write :: (Eq v, Hashable v, MonadIO m) => TempObject v -> v -> v -> m (Write v)
write ref k v = liftIO $ IORef.atomicModifyIORef' ref $
    \known@(Known obj negatives) -> case HMS.lookup k obj of
        Nothing           -> (Known (HMS.insert k v obj) negatives, Ok)
        Just v' | v == v' -> (known, Duplicate)
        Just v'           -> (known, Inconsistent v')
{-# INLINE write #-}

read :: MonadIO m => TempObject v -> m (HMS.HashMap v v)
read = fmap (\(Known known _) -> known) . liftIO . IORef.readIORef
{-# INLINE read #-}

-- | Mark a value as not being part of the object.
negative :: (Hashable v, Ord v, MonadIO m) => TempObject v -> v -> m ()
negative ref k = liftIO $ IORef.atomicModifyIORef' ref $
    \(Known obj negatives) -> (Known obj (Cache.insert k () negatives), ())

isKnownNegative :: (Hashable v, Ord v, MonadIO m) => TempObject v -> v -> m Bool
isKnownNegative ref k = liftIO $ IORef.atomicModifyIORef' ref $
    \known@(Known obj negatives) -> case Cache.lookup k negatives of
        Nothing              -> (known, False)
        Just (_, negatives') -> (Known obj negatives', True)
