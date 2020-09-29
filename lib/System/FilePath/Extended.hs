{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
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
