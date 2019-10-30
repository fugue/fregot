-- | This module holds the modification times for file paths, and get get you a
-- list of files that have been modified since.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Fregot.Repl.MTimes
    ( Handle
    , withHandle
    , watch
    , pop
    ) where

import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Control.Exception       (Exception, throwIO)
import           Control.Monad           (unless, void)
import           Data.Foldable           (for_)
import           Data.Hashable           (Hashable)
import qualified Data.HashMap.Strict     as HMS
import           Data.IORef.Extended     (IORef)
import qualified Data.IORef.Extended     as IORef
import qualified System.Directory        as Dir
import           System.FilePath         (takeDirectory)
import qualified System.FSNotify         as FSNotify


--------------------------------------------------------------------------------
-- Simple canonical filepath representations.

newtype CanonFile = CanonFile {canonFilePath :: FilePath}
    deriving (Eq, Hashable)

newtype CanonDir = CanonDir {canonDirPath :: FilePath}
    deriving (Eq, Hashable)

canonicalize :: FilePath -> IO (CanonDir, CanonFile)
canonicalize path = do
    exists <- Dir.doesFileExist path
    unless exists $ throwIO $ MTimesException $ path ++ " does not exist"
    canonFile <- Dir.canonicalizePath path
    canonDir  <- Dir.canonicalizePath $ takeDirectory path
    return (CanonDir canonDir, CanonFile {canonFilePath = canonFile})


--------------------------------------------------------------------------------
-- Error type.

data MTimesException = MTimesException String

instance Exception MTimesException
instance Show MTimesException where show (MTimesException msg) = msg


--------------------------------------------------------------------------------
-- Actual handle.

data Handle = Handle
    { -- | The manager for FSNotify.
      hManager     :: FSNotify.WatchManager
    , -- | We need to install a watcher for every directory that has files we
      -- care about.
      hDirectories :: MVar (HMS.HashMap CanonDir FSNotify.StopListening)
    , -- | All the files we care about.  The key is the canonical path and the
      -- value is the path is the user specified it, so we can use "prettier"
      -- paths.
      hFiles       :: IORef (HMS.HashMap CanonFile FilePath)
    , -- | Set of files that have been modified but not yet consumed.
      hBuffer      :: IORef (HMS.HashMap CanonFile FilePath)
    , -- | MVar used as a signal that new changes are available.
      hSignal      :: MVar ()
    }

withHandle :: (Handle -> IO a) -> IO a
withHandle f = FSNotify.withManager $ \hManager -> do
    hDirectories <- MVar.newMVar HMS.empty
    hFiles       <- IORef.newIORef HMS.empty
    hBuffer      <- IORef.newIORef HMS.empty
    hSignal      <- MVar.newEmptyMVar
    f Handle {..}

-- | Watch a file for changes.  This is idempotent and will not change anything
-- if the file is already being watched.
watch :: Handle -> FilePath -> IO ()
watch h@Handle {..} path = do
    -- Make sure we're not dealing with duplicates.
    (dir, file) <- canonicalize path

    -- Record that we care about this file in particular.
    IORef.atomicModifyIORef_ hFiles (HMS.insert file path)

    -- Register a watcher for the directory, if it's not already being watched.
    MVar.modifyMVar_ hDirectories $ \dirs -> case HMS.lookup dir dirs of
        Just _   -> return dirs
        Nothing  -> do
            sl <- FSNotify.watchDir hManager
                (canonDirPath dir) (const True) (notifyHandler h)
            return $ HMS.insert dir sl dirs

-- | Called whenever a file changes.
notifyHandler :: Handle -> FSNotify.Event -> IO ()
notifyHandler Handle {..} event = do
    -- FSNotify docs claim the path will already be canonical.
    let canon = CanonFile (FSNotify.eventPath event)
    relevant <- HMS.lookup canon <$> IORef.readIORef hFiles
    for_ relevant $ \path -> do
        -- Add the file to the buffer, and use 'MVar.tryPutMVar' to signal only
        -- if the signal is not on already.
        IORef.atomicModifyIORef_ hBuffer $ HMS.insert canon path
        void $ MVar.tryPutMVar hSignal ()

-- | Pop all changed files.
pop :: Handle -> IO [FilePath]
pop Handle {..} = IORef.atomicModifyIORef hBuffer $
    \buff -> (HMS.empty, map snd $ HMS.toList buff)
