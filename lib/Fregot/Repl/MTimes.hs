-- | This module holds the modification times for file paths, and get get you a
-- list of files that have been modified since.
module Fregot.Repl.MTimes
    ( Handle
    , newHandle
    , tickle
    , modified
    ) where

import           Control.Monad       (filterM)
import qualified Data.HashMap.Strict as HMS
import qualified Data.IORef.Extended as IORef
import qualified Data.Time           as Time
import           System.Directory    (getModificationTime, canonicalizePath)

newtype Handle = Handle
    (IORef.IORef (HMS.HashMap FilePath (FilePath, Time.UTCTime)))

newHandle :: IO Handle
newHandle = Handle <$> IORef.newIORef HMS.empty

tickle :: Handle -> FilePath -> IO ()
tickle (Handle ref) path = do
    time  <- getModificationTime path
    canon <- canonicalizePath path
    IORef.atomicModifyIORef_ ref $ HMS.insert canon (path, time)

modified :: Handle -> IO [FilePath]
modified (Handle ref) = do
    files <- IORef.readIORef ref
    map fst <$> filterM check (map snd $ HMS.toList files)
  where
    check (path, old) = do
        new <- getModificationTime path
        return $ new > old
