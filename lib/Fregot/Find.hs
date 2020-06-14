-- | Find rego files in directories.
module Fregot.Find
    ( findRegoFiles
    , findPrefixedRegoFiles
    ) where

import           Control.Monad.Extended (ifM)
import qualified System.Directory       as Directory
import qualified System.Directory.Find  as Find
import           System.FilePath        ((</>))
import Fregot.Names

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
