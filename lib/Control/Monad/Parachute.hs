{-# LANGUAGE DeriveFunctor #-}
module Control.Monad.Parachute
    ( ParachuteT (..)
    , runParachuteT

    , fatal
    , tellErrors
    ) where

data ParachuteResult e a
    = Ok !a
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

runParachuteT :: Monad m => ParachuteT e m a -> m ([e], Maybe a)
runParachuteT p = do
    (errors, ma) <- unParachuteT p []
    case ma of
        Fatal -> return (errors, Nothing)
        Ok x  -> return (errors, Just x)

fatal :: Monad m => e -> ParachuteT e m a
fatal x = ParachuteT $ \errors -> return (x : errors, Fatal)

tellErrors :: Monad m => [e] -> ParachuteT e m ()
tellErrors es = ParachuteT $ \errors -> return (es ++ errors, Ok ())
