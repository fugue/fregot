module Data.List.Extended
    ( module Data.List
    , unsnoc
    ) where

import           Data.List

unsnoc :: [a] -> Maybe ([a], a)
unsnoc = go []
  where
    go (x : acc) []       = Just (reverse acc, x)
    go []        []       = Nothing
    go acc       (x : xs) = go (x : acc) xs
