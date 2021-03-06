{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
module Data.Yaml.Extended
    ( module Data.Yaml
    , loadJsonOrYaml
    ) where

import qualified Data.Aeson      as Aeson
import           Data.Bifunctor  (first)
import           Data.Yaml
import           System.FilePath (takeExtension)

yamlExtension :: FilePath -> Bool
yamlExtension = (`elem` [".yaml", ".yml"]) . takeExtension

loadJsonOrYaml :: Aeson.FromJSON a => FilePath -> IO (Either String a)
loadJsonOrYaml path
    | yamlExtension path = first show <$> decodeFileEither path
    | otherwise          = Aeson.eitherDecodeFileStrict' path
