{-# LANGUAGE DeriveFunctor #-}
module Control.Monad.Stream
    ( Stream

    , filter
    , collapse
    , peek
    , suspend

    , toList

    , Step (..)
    , step
    ) where

import           Control.Monad       (join)
import           Control.Monad.Trans (MonadIO (..))
import           Prelude             hiding (filter)

newtype Stream i m a = Stream {unStream :: m (SStep i m a)}

data SStep i m a
    = SYield   !a (Stream i m a)
    | SSuspend !i (Stream i m a)
    | SDone
    -- The 'SSingle' constructor is not really necessary since we can also
    -- represent this using 'SYield x (Stream (return SDone))'.  However, since
    -- we deal we deal with singletons so often, this makes our code much
    -- faster.
    | SSingle  !a

pureStream :: Monad m => a -> Stream i m a
pureStream x = Stream $! pure $! SSingle x
{-# INLINE pureStream #-}
{-# SPECIALIZE pureStream :: a -> Stream i IO a #-}

mapStream :: Monad m => (a -> b) -> Stream i m a -> Stream i m b
mapStream f (Stream mxstep) = Stream $ do
    xstep <- mxstep
    case xstep of
        SDone         -> return SDone
        SSuspend i xs -> return $! SSuspend i (mapStream f xs)
        SYield   x xs -> return $! SYield (f x) (mapStream f xs)
        SSingle  x    -> return $! SSingle (f x)
{-# INLINE mapStream #-}
{-# SPECIALIZE mapStream :: (a -> b) -> Stream i IO a -> Stream i IO b #-}

bindStream :: Monad m => Stream i m a -> (a -> Stream i m b) -> Stream i m b
bindStream (Stream mxstep) f = Stream $ do
    xstep <- mxstep
    case xstep of
        SDone         -> return SDone
        SSuspend i xs -> return $! SSuspend i (xs `bindStream` f)
        SYield x xs   -> unStream $ appendStream (f x) (xs `bindStream` f)
        SSingle x     -> unStream (f x)
{-# INLINE bindStream #-}
{-# SPECIALIZE bindStream
    :: Stream i IO a -> (a -> Stream i IO b) -> Stream i IO b #-}

emptyStream :: Monad m => Stream i m a
emptyStream = Stream (pure SDone)
{-# INLINE emptyStream #-}
{-# SPECIALIZE emptyStream :: Stream i IO a #-}

appendStream :: Monad m => Stream i m a -> Stream i m a -> Stream i m a
appendStream (Stream mlstep) right = Stream $ do
    lstep <- mlstep
    case lstep of
        SDone              -> unStream right
        SSuspend i lstream -> return $! SSuspend i (appendStream lstream right)
        SYield x lstream   -> return $! SYield x (appendStream lstream right)
        SSingle x          -> return $! SYield x right
{-# INLINE appendStream #-}
{-# SPECIALIZE appendStream
    :: Stream i IO a -> Stream i IO a -> Stream i IO a #-}

instance Monad m => Semigroup (Stream i m a) where
    (<>) = appendStream
    {-# INLINE (<>) #-}
    {-# SPECIALIZE (<>) :: Stream i IO a -> Stream i IO a -> Stream i IO a #-}

instance Monad m => Monoid (Stream i m a) where
    mempty = emptyStream
    {-# INLINE mempty #-}
    {-# SPECIALIZE mempty :: Stream i IO a #-}

instance Monad m => Functor (Stream i m) where
    fmap = mapStream
    {-# INLINE fmap #-}
    {-# SPECIALIZE fmap :: (a -> b) -> Stream i IO a -> Stream i IO b #-}

instance Monad m => Applicative (Stream i m) where
    pure = pureStream
    {-# INLINE pure #-}
    {-# SPECIALIZE pure :: a -> Stream i IO a #-}
    fs <*> xs = join (fmap (\f -> xs >>= return . f) fs)
    {-# INLINE (<*>) #-}
    {-# SPECIALIZE (<*>)
        :: Stream i IO (a -> b) -> Stream i IO a -> Stream i IO b #-}

instance Monad m => Monad (Stream i m) where
    (>>=) = bindStream
    {-# INLINE (>>=) #-}
    {-# SPECIALIZE (>>=)
        :: Stream i IO a -> (a -> Stream i IO b) -> Stream i IO b #-}

instance MonadIO m => MonadIO (Stream i m) where
    liftIO io = Stream $ do
        x <- liftIO io
        return $! SSingle x
    {-# INLINE liftIO #-}
    {-# SPECIALIZE liftIO :: IO a -> Stream i IO a #-}

filter :: Monad m => (a -> Bool) -> Stream i m a -> Stream i m a
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
{-# SPECIALIZE filter :: (a -> Bool) -> Stream i IO a -> Stream i IO a #-}

collapse :: Monad m => Stream i m a -> Stream i m [a]
collapse = go []
  where
    go acc (Stream mstep) = Stream $ do
        xstep <- mstep
        case xstep of
            SDone        -> return $! SSingle $ reverse acc
            SSingle  x   -> return $! SSingle $ reverse (x : acc)
            SSuspend i s -> return $! SSuspend i $ go acc s
            SYield   x s -> unStream $ go (x : acc) s
{-# INLINE collapse #-}
{-# SPECIALIZE collapse :: Stream i IO a -> Stream i IO [a] #-}

-- | Check if a stream is empty.  If it is not, this returns a new stream, so we
-- don't have to unnecessarily duplicate effects.
peek :: Monad m => Stream i m a -> Stream i m (Maybe (Stream i m a))
peek (Stream mstep) = Stream $ do
    xstep <- mstep
    case xstep of
        SDone          -> return $! SSingle Nothing
        SSuspend i s   -> return $! SSuspend i $ peek s
        s@(SYield _ _) -> return $! SSingle (Just $ Stream $ return s)
        s@(SSingle _)  -> return $! SSingle (Just $ Stream $ return s)
{-# SPECIALIZE peek
    :: Stream i IO a -> Stream i IO (Maybe (Stream i IO a)) #-}

suspend :: Monad m => i -> Stream i m a -> Stream i m a
suspend i x = Stream $ return $ SSuspend i x
{-# INLINE suspend #-}
{-# SPECIALIZE suspend :: i -> Stream i IO a -> Stream i IO a #-}

toList :: Monad m => Stream i m a -> m [a]
toList (Stream mstep) = do
    xstep <- mstep
    case xstep of
        SDone             -> return []
        SSuspend _ stream -> toList stream
        SYield x stream   -> (x :) <$> toList stream
        SSingle x         -> return [x]
{-# SPECIALIZE toList :: Stream i IO a -> IO [a] #-}

data Step i m a
    = Yield   a (Stream i m a)
    | Suspend i (Stream i m a)
    | Done

step :: Monad m => Stream i m a -> m (Step i m a)
step (Stream mstep) = do
    xstep <- mstep
    case xstep of
        SDone         -> return Done
        SYield   x xs -> return $! Yield x xs
        SSuspend i xs -> return $! Suspend i xs
        SSingle  x    -> return $! Yield x mempty
{-# SPECIALIZE step :: Stream i IO a -> IO (Step i IO a) #-}
