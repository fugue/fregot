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
    , insert
    , lookup
    ) where

import           Control.Lens        ((&), (.~), (^.))
import           Control.Lens.TH     (makeLenses)
import qualified Data.Cache          as C
import           Data.Hashable       (Hashable)
import           Data.IORef.Extended (IORef)
import qualified Data.IORef.Extended as IORef
import           GHC.Generics        (Generic)
import           Prelude             hiding (lookup)

-- | We just need to be able to bump this.
type Version = Int

data Versioned k = Versioned {-# UNPACK #-} !Version !k
    deriving (Eq, Generic, Ord)

instance Hashable k => Hashable (Versioned k)

data Cache k v = Cache
    { _cache   :: !(IORef (C.Cache (Versioned k) v))
    , _next    :: !(IORef Version)
    , _version :: !Version
    , _enabled :: !Bool
    }

$(makeLenses ''Cache)

new :: IO (Cache k v)
new = Cache
    <$> IORef.newIORef (C.empty 100)
    <*> IORef.newIORef 1
    <*> pure 0
    <*> pure True

-- | Obtain a new cache version.
bump :: Cache k v -> IO (Cache k v)
bump c = IORef.atomicModifyIORef' (c ^. next) $ \n -> (succ n, c & version .~ n)

-- | Disable the cache.  Used during debugging.
disable :: Cache k v -> Cache k v
disable = enabled .~ False

-- | Add a new row.
insert :: (Hashable k, Ord k) => Cache k v -> k -> v -> IO ()
insert c _ _ | not (c ^. enabled) = pure ()
insert c k val = IORef.atomicModifyIORef_ (c ^. cache) $
    C.insert (Versioned (c ^. version) k) val

lookup :: (Hashable k, Ord k) => Cache k v -> k -> IO (Maybe v)
lookup c _ | not (c ^. enabled) = pure Nothing
lookup c k = IORef.atomicModifyIORef' (c ^. cache) $ \c0 ->
    case C.lookup (Versioned (c ^. version) k) c0 of
        Just (v, c1) -> (c1, Just v)
        _            -> (c0, Nothing)
