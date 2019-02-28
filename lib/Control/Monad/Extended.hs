{-# LANGUAGE BangPatterns #-}
module Control.Monad.Extended
    ( module Control.Monad

    , ifM
    , whenM
    , unlessM
    , unzipWithM
    , foldMapM
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
