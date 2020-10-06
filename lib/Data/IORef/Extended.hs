{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
module Data.IORef.Extended
    ( module Data.IORef
    , atomicModifyIORef_
    ) where

import           Data.IORef

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f = atomicModifyIORef' ref (\x -> (f x, ()))
