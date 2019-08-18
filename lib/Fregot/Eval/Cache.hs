-- | Cache rule result documents.
--
-- The base cache structure that we are using is defined in the 'Data.Cache'
-- module.  Here, we add a simple IO layer, and versioning.
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Fregot.Eval.Cache
    ( Version
    , Cache
    , new
    , bump
    , insert
    , lookup
    ) where

import           Control.Lens        (over, (^.))
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
    , _version :: !Version
    }

$(makeLenses ''Cache)

new :: IO (Cache k v)
new = Cache <$> IORef.newIORef (C.empty 100) <*> pure 0

-- | Obtain a new cache version.
bump :: Cache k v -> Cache k v
bump = over version succ

-- | Add a new row.
insert :: (Hashable k, Ord k) => Cache k v -> k -> v -> IO ()
insert c k val = IORef.atomicModifyIORef_ (c ^. cache) $
    C.insert (Versioned (c ^. version) k) val

lookup :: (Hashable k, Ord k) => Cache k v -> k -> IO (Maybe v)
lookup c k = IORef.atomicModifyIORef' (c ^. cache) $ \c0 ->
    case C.lookup (Versioned (c ^. version) k) c0 of
        Just (v, c1) -> (c1, Just v)
        _            -> (c0, Nothing)
