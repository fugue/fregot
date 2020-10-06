{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

This module holds the modification times for file paths, and get get you a
list of files that have been modified since.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Fregot.Repl.FileWatch
    ( Config (..)
    , Handle
    , withHandle
    , listenersEnabled
    , watch
    , unwatch
    , pop
    , listen
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.MVar  (MVar)
import qualified Control.Concurrent.MVar  as MVar
import           Control.Exception        (Exception, throwIO)
import           Control.Monad            (forever, unless, void)
import           Data.Foldable            (for_)
import           Data.Hashable            (Hashable)
import qualified Data.HashMap.Strict      as HMS
import           Data.IORef.Extended      (IORef)
import qualified Data.IORef.Extended      as IORef
import qualified System.Directory         as Dir
import           System.FilePath          (takeDirectory)
import qualified System.FSNotify          as FSNotify
import qualified System.IO                as IO


--------------------------------------------------------------------------------
-- Simple canonical filepath representations.

newtype CanonFile = CanonFile {canonFilePath :: FilePath}
    deriving (Eq, Hashable)

newtype CanonDir = CanonDir {canonDirPath :: FilePath}
    deriving (Eq, Hashable)

canonicalize :: FilePath -> IO (CanonDir, CanonFile)
canonicalize path = do
    exists <- Dir.doesFileExist path
    unless exists $ throwIO $ FileWatchException $ path ++ " does not exist"
    canonFile <- Dir.canonicalizePath path
    canonDir  <- Dir.canonicalizePath $ takeDirectory path
    return (CanonDir canonDir, CanonFile {canonFilePath = canonFile})


--------------------------------------------------------------------------------
-- Error type.

data FileWatchException = FileWatchException String

instance Exception FileWatchException
instance Show FileWatchException where show (FileWatchException msg) = msg


--------------------------------------------------------------------------------
-- Actual handle.

data Config = Config
    { -- | If listeners are enabled, the registered listeners will be called
      -- when files change.  If listeners are disabled, they will be ignored and
      -- you must manually check for changes using 'pop'.
      cEnableListeners :: Bool
    }

data Handle meta = Handle
    { -- | Configuration.
      hConfig      :: Config
    , -- | The manager for FSNotify.
      hManager     :: FSNotify.WatchManager
    , -- | We need to install a watcher for every directory that has files we
      -- care about.
      hDirectories :: MVar (HMS.HashMap CanonDir FSNotify.StopListening)
    , -- | All the files we care about.  The key is the canonical path and the
      -- value is the path is the user specified it, so we can use "prettier"
      -- paths.  We also allow the user to store a bit of static metadata as
      -- well.
      hFiles       :: IORef (HMS.HashMap CanonFile (CanonDir, FilePath, meta))
    , -- | Set of files that have been modified but not yet consumed.
      hBuffer      :: IORef (HMS.HashMap CanonFile (FilePath, meta))
    , -- | MVar used as a signal that new changes are available.
      hSignal      :: MVar ()
      -- | Listeners that need to be notified of file changes.  Litteners are
      -- called one after the other.
    , hListeners   :: IORef [[(FilePath, meta)] -> IO ()]
    }

withHandle :: Config -> (Handle meta -> IO a) -> IO a
withHandle hConfig f = FSNotify.withManager $ \hManager -> do
    hDirectories <- MVar.newMVar HMS.empty
    hFiles       <- IORef.newIORef HMS.empty
    hBuffer      <- IORef.newIORef HMS.empty
    hSignal      <- MVar.newEmptyMVar
    hListeners   <- IORef.newIORef []

    let h = Handle {..}
    withListenWorker h (f h)

listenersEnabled :: Handle meta -> Bool
listenersEnabled = cEnableListeners . hConfig

-- | Watch a file for changes.  This is idempotent and will not change anything
-- if the file is already being watched.
watch :: Handle meta -> FilePath -> meta -> IO ()
watch h@Handle {..} path meta = do
    -- Make sure we're not dealing with duplicates.
    (dir, file) <- canonicalize path

    -- Record that we care about this file in particular.
    IORef.atomicModifyIORef_ hFiles (HMS.insert file (dir, path, meta))

    -- Register a watcher for the directory, if it's not already being watched.
    MVar.modifyMVar_ hDirectories $ \dirs -> case HMS.lookup dir dirs of
        Just _   -> return dirs
        Nothing  -> do
            sl <- FSNotify.watchDir hManager
                (canonDirPath dir) (const True) (notifyHandler h)
            return $ HMS.insert dir sl dirs

-- | Stop watching a file.
unwatch :: Handle meta -> FilePath -> IO ()
unwatch Handle {..} path = do
    -- Remove the file from the set.  Calculate if other files also refer to
    -- this directory.  If they don't, stop listening.
    (dir, file) <- canonicalize path
    keepDir     <- IORef.atomicModifyIORef' hFiles $ \files0 ->
        let files1  = HMS.delete file files0
            keepDir = any (\(d, _, _) -> d == dir) files1 in
        (files1, keepDir)
    unless keepDir $ MVar.modifyMVar_ hDirectories $ \dirs ->
        case HMS.lookup dir dirs of
            Nothing   -> pure dirs
            Just stop -> stop >> pure (HMS.delete dir dirs)

-- | Called whenever a file changes.
notifyHandler :: Handle meta -> FSNotify.Event -> IO ()
notifyHandler Handle {..} (event@(FSNotify.Modified {})) = do
    -- FSNotify docs claim the path will already be canonical.
    let canon = CanonFile (FSNotify.eventPath event)
    relevant <- HMS.lookup canon <$> IORef.readIORef hFiles
    for_ relevant $ \(_dir, path, meta) -> do
        -- Add the file to the buffer, and use 'MVar.tryPutMVar' to signal only
        -- if the signal is not on already.
        IORef.atomicModifyIORef_ hBuffer $ HMS.insert canon (path, meta)
        void $ MVar.tryPutMVar hSignal ()
notifyHandler _ _ = pure ()

-- | Pop all changed files.
pop :: Handle meta -> IO [(FilePath, meta)]
pop Handle {..} = IORef.atomicModifyIORef hBuffer $
    \buff -> (HMS.empty, map snd $ HMS.toList buff)

-- | Add a listener that gets called on (possibly buffered) file changes.
listen :: Handle meta -> ([(FilePath, meta)] -> IO ()) -> IO ()
listen Handle {..} l
    | cEnableListeners hConfig = IORef.atomicModifyIORef_ hListeners (l :)
    | otherwise                = pure ()

-- | Start a worker that watches for signals and calls the listeners.
withListenWorker :: Handle meta -> IO a -> IO a
withListenWorker h@Handle {..}
    | cEnableListeners hConfig = Async.withAsync worker . const
    | otherwise                = id
  where
    worker = forever $ do
        -- Wait for signal and then get the changed files.
        MVar.takeMVar hSignal
        popped <- pop h

        -- Call listeners one after the other.
        unless (null popped) $ do
            listeners <- IORef.readIORef hListeners
            for_ listeners $ \listener -> do
                async <- Async.async $ listener popped
                Async.waitCatch async >>= either (IO.hPrint IO.stderr) pure
