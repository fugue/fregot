-- | Cache rule result documents.
{-# LANGUAGE TemplateHaskell #-}
module Fregot.Eval.Cache
    ( Version
    , Cache
    , new
    , bump
    , insert
    , lookup
    ) where

import           Control.Lens        ((^.))
import           Control.Lens.TH     (makeLenses)
import qualified Data.Cache          as C
import           Data.Hashable       (Hashable)
import           Data.IORef.Extended (IORef)
import qualified Data.IORef.Extended as IORef
import           Prelude             hiding (lookup)

type Version = Int

data Cache k v = Cache
    { _cache   :: !(IORef (C.Cache (k, Version) v))
    , _version :: !(IORef Version)
    }

$(makeLenses ''Cache)

new :: IO (Cache k v)
new = Cache <$> IORef.newIORef (C.empty 100) <*> IORef.newIORef 0

bump :: Cache k v -> IO Version
bump c = IORef.atomicModifyIORef' (c ^. version) $ \v -> (succ v, v)

insert :: (Hashable k, Ord k) => Cache k v -> (k, Version) -> v -> IO ()
insert c k v = IORef.atomicModifyIORef_ (c ^. cache) $ C.insert k v

lookup :: (Hashable k, Ord k) => Cache k v -> (k, Version) -> IO (Maybe v)
lookup c k = IORef.atomicModifyIORef' (c ^. cache) $ \pc ->
    case C.lookup k pc of
        Nothing       -> (pc, Nothing)
        Just (v, pc') -> (pc', Just v)
