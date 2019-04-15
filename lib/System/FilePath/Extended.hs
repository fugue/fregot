module System.FilePath.Extended
    ( module System.FilePath
    , listExtensions
    ) where

import           System.FilePath

listExtensions :: FilePath -> [String]
listExtensions = go [] . reverse
  where
    go acc revpath = case break isExtSeparator revpath of
        (ext, _sep : more) -> go (reverse ext : acc) more
        (_,   [])          -> reverse acc
