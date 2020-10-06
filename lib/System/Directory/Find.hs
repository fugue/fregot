{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
module System.Directory.Find
    ( recursivelyFindPaths
    , recursivelyFindFiles
    , recursivelyFindFilesWithExtension
    , recursivelyFindFilesWithExtensions
    , glob
    ) where

import           Control.Monad        (forM_, when)
import           Control.Monad.Trans  (liftIO)
import           Control.Monad.Writer (WriterT, execWriterT, tell)
import           Data.List            (sort)
import           System.Directory
import           System.FilePath
import qualified System.FilePath.Glob as Glob

-- | Recursively find files in a directory using an extension.  The returned
-- file paths /DO NOT/ include the directory prefix.
recursivelyFindFilesWithExtension
    :: String                 -- ^ Extension (including the '.').
    -> FilePath               -- ^ Initial search directory.
    -> IO [FilePath]          -- ^ Found files.
recursivelyFindFilesWithExtension =
    recursivelyFindFilesWithExtensions . pure

-- | Recursively find files in a directory using an extension.  The returned
-- file paths /DO NOT/ include the directory prefix.
recursivelyFindFilesWithExtensions
    :: [String]               -- ^ Extensions (including the '.').
    -> FilePath               -- ^ Initial search directory.
    -> IO [FilePath]          -- ^ Found files.
recursivelyFindFilesWithExtensions exts =
    recursivelyFindFiles (return . (`elem` exts) . takeExtension)

-- | Recursively find files in a directory using a predicate.  The returned file
-- paths /DO NOT/ include the initial directory prefix.
recursivelyFindFiles
    :: (FilePath -> IO Bool)  -- ^ Predicate on paths.
    -> FilePath               -- ^ Initial search directory.
    -> IO [FilePath]          -- ^ Found files.
recursivelyFindFiles predicate = recursivelyFindPaths $ \fn -> do
    isFile <- doesFileExist fn
    if isFile then predicate fn else return False

-- | Recursively find paths in a directory using a predicate.  The returned file
-- paths /DO NOT/ include the directory prefix.  We also /DO NOT/ search hidden
-- files and directories.
recursivelyFindPaths
    :: (FilePath -> IO Bool)  -- ^ Predicate on paths.
    -> FilePath               -- ^ Initial search directory.
    -> IO [FilePath]          -- ^ Found files.
recursivelyFindPaths predicate dir0 = do
  isDir <- liftIO $ doesDirectoryExist dir0
  if isDir
    then execWriterT (go dir0 "")
    else return []
  where
    go :: FilePath -> FilePath -> WriterT [FilePath] IO ()
    go path pretty = do
        match <- liftIO $ predicate path
        when match $ tell [if null pretty then "." else pretty]

        isDir <- liftIO $ doesDirectoryExist path
        when isDir $ do
            contents <- liftIO $
                sort . filter search <$> getDirectoryContents path
            forM_ contents $ \c -> go (path </> c) (pretty </> c)

    search ('.' : _) = False
    search _         = True

--------------------------------------------------------------------------------
-- | Perform a glob match in the current directory.
--
-- This is a drop-in replacement for `glob` from the `Glob` library, which has a
-- an annoying tendency to return absolute file paths.
glob :: String -> IO [FilePath]
glob pattern =
    map dropLeadingDot <$> Glob.globDir1 (Glob.compile pattern) "."
  where
    dropLeadingDot fp0 = case break isPathSeparator fp0 of
        (".", fp1) -> drop 1 fp1
        _          -> fp0
