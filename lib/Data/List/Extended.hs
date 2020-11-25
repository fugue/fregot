{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
module Data.List.Extended
    ( module Data.List
    , unsnoc
    , splits
    , maybeLast
    , maybeInitLast
    ) where

import           Data.List

unsnoc :: [a] -> Maybe ([a], a)
unsnoc = go []
  where
    go (x : acc) []       = Just (reverse acc, x)
    go []        []       = Nothing
    go acc       (x : xs) = go (x : acc) xs

-- | Like 'inits', but also gives you the tail.
splits :: [a] -> [([a], [a])]
splits = go []
  where
    go acc l = (reverse acc, l) : case l of
        []       -> []
        (x : xs) -> go (x : acc) xs

maybeLast :: [a] -> Maybe a
maybeLast l = if null l then Nothing else Just (last l)

maybeInitLast :: [a] -> Maybe ([a], a)
maybeInitLast l = if null l then Nothing else Just (init l, last l)
