module Data.IORef.Extended
    ( module Data.IORef
    , atomicModifyIORef_
    ) where

import           Data.IORef

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f = atomicModifyIORef' ref (\x -> (f x, ()))
