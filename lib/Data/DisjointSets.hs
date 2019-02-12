module Data.DisjointSets
    ( DisjointSets
    , empty
    , root
    , union
    ) where

import           Data.Hashable       (Hashable (..))
import qualified Data.HashMap.Strict as HMS

data Node k v = Ref !k | Root !v deriving (Show)

newtype DisjointSets k v = DisjointSets (HMS.HashMap k (Node k v))
    deriving (Show)

empty :: DisjointSets k v
empty = DisjointSets HMS.empty

root :: (Eq k, Hashable k, Monoid v) => k -> DisjointSets k v -> (k, v)
root k dj@(DisjointSets m) = case HMS.lookup k m of
    Nothing       -> (k, mempty)
    Just (Ref l)  -> root l dj
    Just (Root v) -> (k, v)

union
    :: (Eq k, Hashable k, Monoid v)
    => k -> k -> DisjointSets k v -> (k, v, DisjointSets k v)
union xk yk dj@(DisjointSets m0)
    | xr == yr  = (xr, xv, dj)
    | otherwise = (xr, xyv, DisjointSets m1)
  where
    (xr, xv) = root xk dj
    (yr, yv) = root yk dj
    xyv      = mappend xv yv
    m1       = HMS.insert xr (Root xyv) $ HMS.insert yr (Ref xr) m0
