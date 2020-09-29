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
    ) where

import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HMS
import qualified Data.IORef          as IORef
import           Prelude             hiding (read)

type TempObject v = IORef.IORef (HMS.HashMap v v)

new :: MonadIO m => HMS.HashMap v v -> m (TempObject v)
new = liftIO . IORef.newIORef
{-# INLINE new #-}

data Write v = Ok | Duplicate | Inconsistent v

-- | Write a key/value into the temporary object.  Returns if the write
-- succeeded (i.e. it was not already there).  May raise an exception if
-- there is already a different value for this key.
write :: (Eq v, Hashable v, MonadIO m) => TempObject v -> v -> v -> m (Write v)
write ref k v = liftIO $ IORef.atomicModifyIORef' ref $
    \obj -> case HMS.lookup k obj of
        Nothing           -> (HMS.insert k v obj, Ok)
        Just v' | v == v' -> (obj, Duplicate)
        Just v'           -> (obj, Inconsistent v')
{-# INLINE write #-}

read :: MonadIO m => TempObject v -> m (HMS.HashMap v v)
read = liftIO . IORef.readIORef
{-# INLINE read #-}
