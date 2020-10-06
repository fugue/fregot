{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE BangPatterns #-}
module Control.Monad.Extended
    ( module Control.Monad

    , ifM
    , whenM
    , unlessM
    , unzipWithM
    , foldMapM
    , mapAccumM
    ) where

import           Control.Monad

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM x l r = do
    b <- x
    if b then l else r

whenM :: Monad m => m Bool -> m () -> m ()
whenM x l = ifM x l (return ())

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM x = whenM (not <$> x)

unzipWithM :: Monad m => (a -> m (b, c)) -> [a] -> m ([b], [c])
unzipWithM f = fmap unzip . mapM f

foldMapM :: (Monoid s, Monad m) => (a -> m s) -> [a] -> m s
foldMapM f = go mempty
  where
    go !acc []       = return acc
    go !acc (x : xs) = do
        s <- f x
        go (acc <> s) xs

mapAccumM :: Monad m => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
mapAccumM step initial = go initial []
  where
    go !acc items []       = return (acc, reverse items)
    go !acc items (x : xs) = do
        (acc', y) <- step acc x
        go acc' (y : items) xs
