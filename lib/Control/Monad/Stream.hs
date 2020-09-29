{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE DeriveFunctor #-}
module Control.Monad.Stream
    ( Stream

    , filter
    , collapse
    , peek
    , suspend
    , throw

    , toList

    , Step (..)
    , step

    , coerce
    , mapError
    ) where

import           Control.Monad       (join)
import           Control.Monad.Trans (MonadIO (..))
import           Data.Void           (Void)
import           Prelude             hiding (filter)
import           Unsafe.Coerce       (unsafeCoerce)

newtype Stream e i m a = Stream {unStream :: m (SStep e i m a)}

data SStep e i m a
    = SYield   !a (Stream e i m a)
    | SSuspend !i (Stream e i m a)
    | SDone
    -- The 'SSingle' constructor is not really necessary since we can also
    -- represent this using 'SYield x (Stream (return SDone))'.  However, since
    -- we deal we deal with singletons so often, this makes our code much
    -- faster.
    | SSingle  !a
    -- At some point, we threw errors just by installing an error handler and
    -- using 'Control.Exception'.  Now; we have a specific error constructor
    -- here which is a bit cleaner.
    | SError   !e

pureStream :: Monad m => a -> Stream e i m a
pureStream x = Stream $! pure $! SSingle x
{-# INLINE pureStream #-}
{-# SPECIALIZE pureStream :: a -> Stream e i IO a #-}

mapStream :: Monad m => (a -> b) -> Stream e i m a -> Stream e i m b
mapStream f (Stream mxstep) = Stream $ do
    xstep <- mxstep
    case xstep of
        SDone         -> return SDone
        SSuspend i xs -> return $! SSuspend i (mapStream f xs)
        SYield   x xs -> return $! SYield (f x) (mapStream f xs)
        SSingle  x    -> return $! SSingle (f x)
        SError   e    -> return $! SError e
{-# INLINE mapStream #-}
{-# SPECIALIZE mapStream :: (a -> b) -> Stream e i IO a -> Stream e i IO b #-}

bindStream
    :: Monad m => Stream e i m a -> (a -> Stream e i m b) -> Stream e i m b
bindStream (Stream mxstep) f = Stream $ do
    xstep <- mxstep
    case xstep of
        SDone         -> return SDone
        SSuspend i xs -> return $! SSuspend i (xs `bindStream` f)
        SYield x xs   -> unStream $ appendStream (f x) (xs `bindStream` f)
        SSingle x     -> unStream (f x)
        SError e      -> return (SError e)
{-# INLINE bindStream #-}
{-# SPECIALIZE bindStream
    :: Stream e i IO a -> (a -> Stream e i IO b) -> Stream e i IO b #-}

emptyStream :: Monad m => Stream e i m a
emptyStream = Stream (pure SDone)
{-# INLINE emptyStream #-}
{-# SPECIALIZE emptyStream :: Stream e i IO a #-}

appendStream :: Monad m => Stream e i m a -> Stream e i m a -> Stream e i m a
appendStream (Stream mlstep) right = Stream $ do
    lstep <- mlstep
    case lstep of
        SDone              -> unStream right
        SSuspend i lstream -> return $! SSuspend i (appendStream lstream right)
        SYield x lstream   -> return $! SYield x (appendStream lstream right)
        SSingle x          -> return $! SYield x right
        SError e           -> return $! SError e
{-# INLINE appendStream #-}
{-# SPECIALIZE appendStream
    :: Stream e i IO a -> Stream e i IO a -> Stream e i IO a #-}

instance Monad m => Semigroup (Stream e i m a) where
    (<>) = appendStream
    {-# INLINE (<>) #-}
    {-# SPECIALIZE (<>)
            :: Stream e i IO a -> Stream e i IO a -> Stream e i IO a #-}

instance Monad m => Monoid (Stream e i m a) where
    mempty = emptyStream
    {-# INLINE mempty #-}
    {-# SPECIALIZE mempty :: Stream e i IO a #-}

instance Monad m => Functor (Stream e i m) where
    fmap = mapStream
    {-# INLINE fmap #-}
    {-# SPECIALIZE fmap :: (a -> b) -> Stream e i IO a -> Stream e i IO b #-}

instance Monad m => Applicative (Stream e i m) where
    pure = pureStream
    {-# INLINE pure #-}
    {-# SPECIALIZE pure :: a -> Stream e i IO a #-}
    fs <*> xs = join (fmap (\f -> xs >>= return . f) fs)
    {-# INLINE (<*>) #-}
    {-# SPECIALIZE (<*>)
        :: Stream e i IO (a -> b) -> Stream e i IO a -> Stream e i IO b #-}

instance Monad m => Monad (Stream e i m) where
    (>>=) = bindStream
    {-# INLINE (>>=) #-}
    {-# SPECIALIZE (>>=)
        :: Stream e i IO a -> (a -> Stream e i IO b) -> Stream e i IO b #-}

instance MonadIO m => MonadIO (Stream e i m) where
    liftIO io = Stream $ do
        x <- liftIO io
        return $! SSingle x
    {-# INLINE liftIO #-}
    {-# SPECIALIZE liftIO :: IO a -> Stream e i IO a #-}

filter :: Monad m => (a -> Bool) -> Stream e i m a -> Stream e i m a
filter f (Stream mstep) = Stream $ do
    xstep <- mstep
    case xstep of
        SDone            -> return SDone
        SSuspend i xs    -> return $! SSuspend i (filter f xs)
        SYield x xs
            | f x       -> return $! SYield x (filter f xs)
            | otherwise -> unStream (filter f xs)
        SSingle x
            | f x       -> return $! SSingle x
            | otherwise -> return SDone
        SError e        -> return $! SError e
{-# SPECIALIZE filter :: (a -> Bool) -> Stream e i IO a -> Stream e i IO a #-}

collapse :: Monad m => Stream e i m a -> Stream e i m [a]
collapse = go []
  where
    go acc (Stream mstep) = Stream $ do
        xstep <- mstep
        case xstep of
            SDone        -> return $! SSingle $ reverse acc
            SSingle  x   -> return $! SSingle $ reverse (x : acc)
            SSuspend i s -> return $! SSuspend i $ go acc s
            SYield   x s -> unStream $ go (x : acc) s
            SError   e   -> return $! SError e
{-# INLINE collapse #-}
{-# SPECIALIZE collapse :: Stream e i IO a -> Stream e i IO [a] #-}

-- | Check if a stream is empty.  If it is not, this returns a new stream, so we
-- don't have to unnecessarily duplicate effects.
peek :: Monad m => Stream e i m a -> Stream e i m (Maybe (Stream e i m a))
peek (Stream mstep) = Stream $ do
    xstep <- mstep
    case xstep of
        SDone          -> return $! SSingle Nothing
        SSuspend i s   -> return $! SSuspend i $ peek s
        s@(SYield _ _) -> return $! SSingle (Just $ Stream $ return s)
        s@(SSingle _)  -> return $! SSingle (Just $ Stream $ return s)
        SError e       -> return $! SError e
{-# SPECIALIZE peek
    :: Stream e i IO a -> Stream e i IO (Maybe (Stream e i IO a)) #-}

suspend :: Monad m => i -> Stream e i m a -> Stream e i m a
suspend i x = Stream $ return $ SSuspend i x
{-# INLINE suspend #-}
{-# SPECIALIZE suspend :: i -> Stream e i IO a -> Stream e i IO a #-}

throw :: Monad m => e -> Stream e i m a
throw = Stream . return . SError
{-# INLINE throw #-}
{-# SPECIALIZE throw :: e -> Stream e i IO a #-}

-- NOTE(jaspervdj): I am a little bit worried about what the effects of this
-- 'Either' are on the global performance; since 'toList' is used in the "main"
-- way of consuming a stream.
toList :: Monad m => Stream e i m a -> m (Either e [a])
toList (Stream mstep) = do
    xstep <- mstep
    case xstep of
        SDone             -> return $ Right []
        SSuspend _ stream -> toList stream
        SYield x stream   -> fmap (x :) <$> toList stream
        SSingle x         -> return $ Right [x]
        SError e          -> return $ Left e
{-# SPECIALIZE toList :: Stream e i IO a -> IO (Either e [a]) #-}

data Step e i m a
    = Yield   a (Stream e i m a)
    | Suspend i (Stream e i m a)
    | Done
    | Error   e

step :: Monad m => Stream e i m a -> m (Step e i m a)
step (Stream mstep) = do
    xstep <- mstep
    case xstep of
        SDone         -> return Done
        SYield   x xs -> return $! Yield x xs
        SSuspend i xs -> return $! Suspend i xs
        SSingle  x    -> return $! Yield x mempty
        SError   e    -> return (Error e)
{-# SPECIALIZE step :: Stream e i IO a -> IO (Step e i IO a) #-}

coerce :: Stream e Void m a -> Stream e i m a
coerce =
    -- This is safe because having the `Void` type here guarantees that a
    -- `SSuspend` can never be constructed.
    unsafeCoerce

mapError :: Monad m => (e -> f) -> Stream e i m a -> Stream f i m a
mapError f (Stream mstep) = Stream $ do
    xstep <- mstep
    case xstep of
        SDone         -> return SDone
        SYield   x xs -> return $! SYield x (mapError f xs)
        SSuspend i xs -> return $! SSuspend i (mapError f xs)
        SSingle  x    -> return $! SSingle x
        SError   e    -> return $! SError (f e)
{-# SPECIALIZE mapError :: (e -> f) -> Stream e i IO a -> Stream f i IO a #-}
