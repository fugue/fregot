{-# LANGUAGE Rank2Types #-}
module Control.Monad.Except.Extended
    ( module Control.Monad.Except
    , catching
    ) where

import           Control.Lens         (Prism', preview)
import           Control.Monad.Except

catching :: MonadError e0 m => Prism' e0 e1 -> m a -> (e1 -> m a) -> m a
catching p f g = catchError f $ \e0 -> case preview p e0 of
    Nothing -> throwError e0
    Just e1 -> g e1
