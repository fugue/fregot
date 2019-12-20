module Data.Memoize
    ( memoize
    ) where

import           Data.Hashable     (Hashable)
import qualified Data.HashMap.Lazy as HML
import qualified Data.IORef        as IORef
import           System.IO.Unsafe  (unsafePerformIO)

memoize :: (Eq k, Hashable k) => (k -> v) -> (k -> v)
memoize f = unsafePerformIO $ do
    ref <- IORef.newIORef HML.empty
    pure $ \k -> unsafePerformIO $ IORef.atomicModifyIORef' ref $ \mem ->
        case HML.lookup k mem of
            Nothing -> let v = f k in (HML.insert k v mem, v)
            Just v  -> (mem, v)
{-# NOINLINE memoize #-}
