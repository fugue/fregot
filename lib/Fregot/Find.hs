{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

Find rego files in directories.
-}
module Fregot.Find
    ( FoundRegoFile
    , findPrefixedRegoFiles
    ) where

import           Control.Monad.Extended (ifM)
import           Fregot.Names
import qualified System.Directory       as Directory
import qualified System.Directory.Find  as Find

-- | Takes a list of files or directories, and returns a full exhaustive list of
-- all found files.
findRegoFiles :: FilePath -> IO [(Maybe FilePath, FilePath)]
findRegoFiles path = ifM
    (Directory.doesDirectoryExist path)
    (zip (repeat $ Just path) <$>
        Find.recursivelyFindFilesWithExtensions extensions path)
    (return [(Nothing, path)])
  where
    extensions = [".rego", ".yaml", ".yml", ".json"]

findPrefixedRegoFiles :: [DestinationPrefix FilePath] -> IO [FoundRegoFile]
findPrefixedRegoFiles =
    fmap (foldMap sequence) . traverse (traverse findRegoFiles)
