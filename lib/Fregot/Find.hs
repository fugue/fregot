{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

Find rego files in directories.
-}
module Fregot.Find
    ( findRegoFiles
    , findPrefixedRegoFiles
    ) where

import           Control.Monad.Extended (ifM)
import           Fregot.Names
import qualified System.Directory       as Directory
import qualified System.Directory.Find  as Find
import           System.FilePath        ((</>))

-- | Takes a list of files or directories, and returns a full exhaustive list of
-- all found files.
findRegoFiles :: FilePath -> IO [FilePath]
findRegoFiles path = ifM
    (Directory.doesDirectoryExist path)
    (map (path </>) <$>
        Find.recursivelyFindFilesWithExtensions extensions path)
    (return [path])
  where
    extensions = [".rego", ".yaml", ".yml", ".json"]

findPrefixedRegoFiles
    :: [DestinationPrefix FilePath] -> IO [DestinationPrefix FilePath]
findPrefixedRegoFiles =
    fmap (concatMap sequenceA) . traverse (traverse findRegoFiles)
