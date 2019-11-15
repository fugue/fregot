-- | Cache rule result documents.
--
-- The base cache structure that we are using is defined in the 'Data.Cache'
-- module.  Here, we add a simple IO layer, and versioning.
--
-- The idea is that we can bump the "version" of the cache and get a completely
-- empty cache.  However, the old entries are still available for parts of the
-- program that have a reference to the old version of the cache.
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Fregot.Eval.Cache
    ( Version
    , Cache
    , new
    , bump
    , disable
    , writeSingleton
    , flushCollection
    , Result (..)
    , read
    ) where

import           Control.Lens           ((&), (.~), (^.))
import           Control.Lens.TH        (makeLenses)
import           Control.Monad          (when)
import           Control.Monad.Trans    (MonadIO, liftIO)
import qualified Data.Cache             as C
import           Data.Hashable          (Hashable)
import qualified Data.HashMap.Strict    as HMS
import           Data.IORef.Extended    (IORef)
import qualified Data.IORef.Extended    as IORef
import qualified Fregot.Eval.TempObject as TempObject
import           GHC.Generics           (Generic)
import           Prelude                hiding (read)

-- | We just need to be able to bump this.
type Version = Int

-- | A versioned key.
data Versioned k = Versioned {-# UNPACK #-} !Version !k
    deriving (Eq, Generic, Ord, Show)

instance Hashable k => Hashable (Versioned k)

-- | Values in the cache.
--
-- NOTE(jaspervdj): Adding a 'None' constructor here to avoid the 'Maybe'
-- indirection may be worth a small speedup.
data Result v
    -- | Cached result from a complete rule, can only be a single value.
    = Singleton !v
    -- | Cached result from a set or object rule.  The value in the map is
    -- always true for sets.
    | Collection !(HMS.HashMap v v)
    -- | Cached result from a set or object rule, that hasn't been completely
    -- evaluated, so some values may not be here.
    | Partial !(TempObject.TempObject v)

data Cache k v = Cache
    { _cache   :: !(IORef (C.Cache (Versioned k) (Result v)))
    , _next    :: !(IORef Version)
    , _version :: !Version
    , _enabled :: !Bool
    }

$(makeLenses ''Cache)

new :: MonadIO m => m (Cache k v)
new = liftIO $ Cache
    <$> IORef.newIORef (C.empty 100)
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
writeSingleton
    :: (Hashable k, Ord k, MonadIO m) => Cache k v -> k -> v -> m ()
writeSingleton c k val = when (c ^. enabled) $ liftIO $
    IORef.atomicModifyIORef_ (c ^. cache) $
    C.insert (Versioned (c ^. version) k) (Singleton val)

-- | Indicate that we've traversed the entire collection, so we can change
-- 'Partial' to 'Collection' if necessary.
flushCollection
    :: (Hashable k, Ord k, Eq v, Hashable v, MonadIO m)
    => Cache k v -> k -> m ()
flushCollection c ck = when (c ^. enabled) $ do
    c0 <- liftIO $ IORef.readIORef (c ^. cache)
    case C.lookup vk c0 of
        Just (Partial pref, _) -> do
            obj <- TempObject.read pref
            liftIO $ IORef.atomicModifyIORef_ (c ^. cache) $
                \c1 -> C.insert vk (Collection obj) c1
        _ -> pure ()
  where
    vk = Versioned (c ^. version) ck

read
    :: (Hashable k, Ord k, MonadIO m)
    => Cache k v -> k -> m (Maybe (Result v))
read c _ | not (c ^. enabled) = pure Nothing
read c k = liftIO $
    IORef.atomicModifyIORef' (c ^. cache) $ \c0 ->
    case C.lookup (Versioned (c ^. version) k) c0 of
        Just (v, c1) -> (c1, Just v)
        _            -> (c0, Nothing)
