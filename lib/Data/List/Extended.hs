module Data.List.Extended
    ( module Data.List
    , unsnoc
    , splits
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
