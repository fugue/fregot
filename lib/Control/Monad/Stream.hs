{-# LANGUAGE DeriveFunctor #-}
module Control.Monad.Stream
    ( Stream

    , filter
    , collapse
    , peek
    , suspend

    , toList
    ) where

import           Control.Monad       (join)
import           Control.Monad.Trans (MonadIO (..))
import           Prelude             hiding (filter)

newtype Stream i m a = Stream {unStream :: m (Step i m a)}
    deriving (Functor)

data Step i m a
    = Yield   !a (Stream i m a)
    | Suspend !i (Stream i m a)
    | Done
    -- The 'Single' constructor is not really necessary since we can also
    -- represent this using 'Yield x (Stream (return Done))'.  However, since we
    -- deal we deal with singletons so often, this makes our code much faster.
    | Single  !a
    deriving (Functor)

pureStream :: Monad m => a -> Stream i m a
pureStream x = Stream $! pure $! Single x
{-# INLINE pureStream #-}
{-# SPECIALIZE pureStream :: a -> Stream i IO a #-}

bindStream :: Monad m => Stream i m a -> (a -> Stream i m b) -> Stream i m b
bindStream (Stream mxstep) f = Stream $ do
    xstep <- mxstep
    case xstep of
        Done         -> return Done
        Suspend i xs -> return $! Suspend i (xs `bindStream` f)
        Yield x xs   -> unStream $ appendStream (f x) (xs `bindStream` f)
        Single x     -> unStream (f x)
{-# INLINE bindStream #-}
{-# SPECIALIZE bindStream
    :: Stream i IO a -> (a -> Stream i IO b) -> Stream i IO b #-}

emptyStream :: Monad m => Stream i m a
emptyStream = Stream (pure Done)
{-# INLINE emptyStream #-}
{-# SPECIALIZE emptyStream :: Stream i IO a #-}

appendStream :: Monad m => Stream i m a -> Stream i m a -> Stream i m a
appendStream (Stream mlstep) right = Stream $ do
    lstep <- mlstep
    case lstep of
        Done              -> unStream right
        Suspend i lstream -> return $! Suspend i (appendStream lstream right)
        Yield x lstream   -> return $! Yield x (appendStream lstream right)
        Single x          -> return $! Yield x right
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
        return $! Single x
    {-# INLINE liftIO #-}
    {-# SPECIALIZE liftIO :: IO a -> Stream i IO a #-}

filter :: Monad m => (a -> Bool) -> Stream i m a -> Stream i m a
filter f (Stream mstep) = Stream $ do
    step <- mstep
    case step of
        Done            -> return Done
        Suspend i xs    -> return $! Suspend i (filter f xs)
        Yield x xs
            | f x       -> return $! Yield x (filter f xs)
            | otherwise -> unStream (filter f xs)
        Single x
            | f x       -> return $! Single x
            | otherwise -> return Done
{-# SPECIALIZE filter :: (a -> Bool) -> Stream i IO a -> Stream i IO a #-}

collapse :: Monad m => Stream i m a -> Stream i m [a]
collapse = go []
  where
    go acc (Stream mstep) = Stream $ do
        step <- mstep
        case step of
            Done        -> return $! Single $ reverse acc
            Single  x   -> return $! Single $ reverse (x : acc)
            Suspend i s -> return $! Suspend i $ go acc s
            Yield   x s -> unStream $ go (x : acc) s
{-# INLINE collapse #-}
{-# SPECIALIZE collapse :: Stream i IO a -> Stream i IO [a] #-}

-- | Check if a stream is empty.  If it is not, this returns a new stream, so we
-- don't have to unnecessarily duplicate effects.
peek :: Monad m => Stream i m a -> Stream i m (Maybe (Stream i m a))
peek (Stream mstep) = Stream $ do
    step <- mstep
    case step of
        Done          -> return $! Single Nothing
        Suspend i s   -> return $! Suspend i $ peek s
        s@(Yield _ _) -> return $! Single (Just $ Stream $ return s)
        s@(Single _)  -> return $! Single (Just $ Stream $ return s)
{-# SPECIALIZE peek
    :: Stream i IO a -> Stream i IO (Maybe (Stream i IO a)) #-}

suspend :: Monad m => i -> Stream i m a -> Stream i m a
suspend i x = Stream $ return $ Suspend i x
{-# INLINE suspend #-}
{-# SPECIALIZE suspend :: i -> Stream i IO a -> Stream i IO a #-}

toList :: Monad m => Stream i m a -> m [a]
toList (Stream mstep) = do
    step <- mstep
    case step of
        Done             -> return []
        Suspend _ stream -> toList stream
        Yield x stream   -> (x :) <$> toList stream
        Single x         -> return [x]
{-# SPECIALIZE toList :: Stream i IO a -> IO [a] #-}
