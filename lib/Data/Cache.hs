{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Cache
    ( Cache
    , empty
    , insert
    , lookup
    ) where

import           Control.Lens    ((%~), (&), (.~), (^.))
import           Control.Lens.TH (makeLenses)
import           Data.Hashable   (Hashable)
import qualified Data.HashPSQ    as HashPSQ
import           Data.Int        (Int64)
import           Data.Maybe      (isNothing)
import           Prelude         hiding (lookup)

type Priority = Int64

data Cache k v = Cache
    { _capacity :: !Int       -- ^ The maximum number of elements in the queue
    , _size     :: !Int       -- ^ The current number of elements in the queue
    , _tick     :: !Priority  -- ^ The next logical time
    , _queue    :: !(HashPSQ.HashPSQ k Priority v)
    } deriving (Eq, Show)

$(makeLenses ''Cache)

empty :: Int -> Cache k v
empty cap
    | cap < 1   = error "Cache.empty: capacity < 1"
    | otherwise = Cache cap 0 0 HashPSQ.empty

trim :: (Hashable k, Ord k) => Cache k v -> Cache k v
trim c
    | c ^. tick == maxBound      = empty (c ^. capacity)
    | c ^. size <= c ^. capacity = c
    | otherwise                  = c
        & size %~ pred
        & queue %~ HashPSQ.deleteMin

insert :: (Hashable k, Ord k) => k -> v -> Cache k v -> Cache k v
insert key val c = trim $!
    let (mbOldVal, q) = HashPSQ.insertView key (c ^. tick) val (c ^. queue)
    in c
        & size %~ (if isNothing mbOldVal then succ else id)
        & tick %~ succ
        & queue .~ q

lookup
    :: (Hashable k, Ord k) => k -> Cache k v -> Maybe (v, Cache k v)
lookup k c =
    case HashPSQ.unsafeLookupIncreasePriority k (c ^. tick) (c ^. queue) of
        (Nothing, _)     -> Nothing
        (Just (_, x), q) ->
            let !c' = trim $ c & tick %~ succ & queue .~ q in Just (x, c')
