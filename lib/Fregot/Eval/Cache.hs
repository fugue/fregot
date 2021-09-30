{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

Cache rule result documents.

The base cache structure that we are using is defined in the 'Data.Cache'
module.  Here, we add a simple IO layer, and versioning.

The idea is that we can bump the "version" of the cache and get a completely
empty cache.  However, the old entries are still available for parts of the
program that have a reference to the old version of the cache.
-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Fregot.Eval.Cache
    ( Version
    , Cache, enabled
    , new
    , bump
    , disable
    , write
    , read
    ) where

import           Control.Lens           ((&), (.~), (^.))
import           Control.Lens.TH        (makeLenses)
import           Control.Monad          (when)
import           Control.Monad.Trans    (MonadIO, liftIO)
import qualified Data.Cache             as C
import           Data.Hashable          (Hashable)
import           Data.IORef.Extended    (IORef)
import qualified Data.IORef.Extended    as IORef
import           GHC.Generics           (Generic)
import           Prelude                hiding (read)

-- | We just need to be able to bump this.
type Version = Int

-- | A versioned key.
data Versioned k = Versioned {-# UNPACK #-} !Version !k
    deriving (Eq, Generic, Ord, Show)

instance Hashable k => Hashable (Versioned k)

data Cache k v = Cache
    { _cache   :: !(IORef (C.Cache (Versioned k) v))
    , _next    :: !(IORef Version)
    , _version :: !Version
    , _enabled :: !Bool
    }

$(makeLenses ''Cache)

new :: MonadIO m => m (Cache k v)
new = liftIO $ Cache
    <$> IORef.newIORef (C.empty $ 100 * 2056)
    <*> IORef.newIORef 1
    <*> pure 0
    <*> pure True

-- | Obtain a new cache version.
bump :: MonadIO m => Cache k v -> m (Cache k v)
bump c = liftIO $
    IORef.atomicModifyIORef' (c ^. next) $ \n -> (succ n, c & version .~ n)

-- | Disable the cache.  Used during debugging.
disable :: Cache k v -> Cache k v
disable = enabled .~ False

-- | Add a new singleton result.
write
    :: (Hashable k, Ord k, MonadIO m) => Cache k v -> k -> v -> m ()
write c k result = when (c ^. enabled) $ liftIO $
    IORef.atomicModifyIORef_ (c ^. cache) $
    C.insert (Versioned (c ^. version) k) result

read :: (Hashable k, Ord k, MonadIO m) => Cache k v -> k -> m (Maybe v)
read c _ | not (c ^. enabled) = pure Nothing
read c k = liftIO $
    IORef.atomicModifyIORef' (c ^. cache) $ \c0 ->
    case C.lookup (Versioned (c ^. version) k) c0 of
        Just (v, c1) -> (c1, Just v)
        _            -> (c0, Nothing)
