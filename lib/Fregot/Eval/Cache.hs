-- | Cache rule result documents.
--
-- The base cache structure that we are using is defined in the 'Data.Cache'
-- module.  Here, we add loads of rule-specific functionality.
--
-- Since we are generally using top-down depth-first evaluation, we cannot just
-- do something like:
--
-- 1.  Evaluate a rule to a list of rows
-- 2.  Store this list
--
-- Evaluating the entire list would make our search strategy breadth first.  In
-- order to avoid this, we want to store the rows one at a time, as they found.
--
--
--
-- NOTE(jaspervdj): This module currently makes the assumptions that rules are
-- not recursive, and that access is single threaded.
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

-- | We just need to be able to bump this.
type Version = Int

data Cache k v = Cache
    { _cache   :: !(IORef (C.Cache (k, Version) v))
    , _version :: !(IORef Version)
    }

$(makeLenses ''Cache)

new :: IO (Cache k v)
new = Cache <$> IORef.newIORef (C.empty 100) <*> IORef.newIORef 0

-- | Obtain a new cache version.
bump :: Cache k v -> IO Version
bump c = IORef.atomicModifyIORef' (c ^. version) $ \v -> (succ v, v)

-- | Add a new row.
insert :: (Hashable k, Ord k) => Cache k v -> (k, Version) -> v -> IO ()
insert c k val = IORef.atomicModifyIORef_ (c ^. cache) $ C.insert k val

lookup :: (Hashable k, Ord k) => Cache k v -> (k, Version) -> IO (Maybe v)
lookup c k = IORef.atomicModifyIORef' (c ^. cache) $ \c0 ->
    case C.lookup k c0 of
        Just (v, c1)  -> (c1, Just v)
        _             -> (c0, Nothing)
