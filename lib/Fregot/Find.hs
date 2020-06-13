-- | Find rego files in directories.
module Fregot.Find
    ( findRegoFiles
    ) where

import           Control.Monad.Extended (forM, ifM)
import qualified System.Directory       as Directory
import qualified System.Directory.Find  as Find
import           System.FilePath        ((</>))

-- | Takes a list of files or directories, and returns a full exhaustive list of
-- all found files.
findRegoFiles :: [FilePath] -> IO [FilePath]
findRegoFiles paths = fmap concat $ forM paths $ \path -> ifM
    (Directory.doesDirectoryExist path)
    (map (path </>) <$>
        Find.recursivelyFindFilesWithExtensions extensions path)
    (return [path])
  where
    extensions = [".rego", ".yaml", ".yml", ".json"]
