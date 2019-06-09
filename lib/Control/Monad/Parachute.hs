{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE UndecidableInstances  #-}
module Control.Monad.Parachute
    ( ParachuteT (..)
    , runParachuteT
    , mapParachuteT

    , fatal
    , dieIfErrors
    , tellErrors
    , tellError

    , try
    , catch
    ) where

import           Control.Monad.Except (MonadError (..))
import           Control.Monad.Reader (MonadReader (..))
import           Control.Monad.Trans  (MonadIO (..))

data ParachuteResult e a
    = Ok a
    | Fatal
    deriving (Functor)

newtype ParachuteT e m a = ParachuteT
    { unParachuteT :: [e] -> m ([e], ParachuteResult e a)
    } deriving (Functor)

instance Monad m => Applicative (ParachuteT e m) where
    pure x = ParachuteT $ \errors -> pure (errors, Ok x)
    ParachuteT f <*> ParachuteT g = ParachuteT $ \errors0 -> do
        (errors1, mh) <- f errors0
        (errors2, ma) <- g errors1
        case (mh, ma) of
            (Ok h, Ok a) -> pure (errors2, Ok (h a))
            _            -> pure (errors2, Fatal)

instance Monad m => Monad (ParachuteT e m) where
    ParachuteT f >>= g = ParachuteT $ \errors0 -> do
        (errors1, ma) <- f errors0
        case ma of
            Fatal -> pure (errors1, Fatal)
            Ok a  -> unParachuteT (g a) errors1

instance MonadIO m => MonadIO (ParachuteT e m) where
    liftIO io = ParachuteT $ \errors -> do
        x <- liftIO io
        pure (errors, Ok x)

instance MonadReader r m => MonadReader r (ParachuteT e m) where
    ask = ParachuteT $ \errors -> do
        x <- ask
        pure (errors, Ok x)

    local f (ParachuteT p) = ParachuteT (local f . p)

instance Monad m => MonadError [e] (ParachuteT e m) where
    throwError = fatals
    catchError = catch

runParachuteT :: Monad m => ParachuteT e m a -> m ([e], Maybe a)
runParachuteT p = do
    (errors, ma) <- unParachuteT p []
    case ma of
        Fatal -> return (errors, Nothing)
        Ok x  -> return (errors, Just x)

-- | Apply a natural transformation on the underlying Monad.
mapParachuteT
    :: (Monad m, Monad n)
    => (forall b. m b -> n b)
    -> ParachuteT e m a -> ParachuteT e n a
mapParachuteT f (ParachuteT p) = ParachuteT (f . p)

fatal :: Monad m => e -> ParachuteT e m a
fatal x = ParachuteT $ \errors -> return (x : errors, Fatal)

fatals :: Monad m => [e] -> ParachuteT e m a
fatals xs = ParachuteT $ \errors -> return (xs ++ errors, Fatal)

-- | If any errors have been registered, die at this point.
dieIfErrors :: Monad m => ParachuteT e m ()
dieIfErrors = ParachuteT $ \errors -> return
    (errors, if null errors then Ok () else Fatal)

tellErrors :: Monad m => [e] -> ParachuteT e m ()
tellErrors es = ParachuteT $ \errors -> return (es ++ errors, Ok ())

tellError :: Monad m => e -> ParachuteT e m ()
tellError e = ParachuteT $ \errors -> return (e : errors, Ok ())

try :: Monad m => ParachuteT e m a -> ParachuteT e m (Either [e] a)
try (ParachuteT mx) = ParachuteT $ \originalErrors -> do
    -- Execute `mx` in isolation.
    (xerrs, xres) <- mx []

    -- Catch fatal errors only (for now).
    case xres of
        Fatal -> return (originalErrors, Ok (Left xerrs))
        Ok x  -> return (xerrs ++ originalErrors, Ok (Right x))

catch
    :: Monad m
    => ParachuteT e m a -> ([e] -> ParachuteT e m a) -> ParachuteT e m a
catch mx f = try mx >>= either f return
