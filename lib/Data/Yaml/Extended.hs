module Data.Yaml.Extended
    ( module Data.Yaml
    , loadJsonOrYaml
    ) where

import qualified Data.Aeson      as Aeson
import           Data.Yaml
import           System.FilePath (takeExtension)
import Data.Bifunctor (first)

yamlExtension :: FilePath -> Bool
yamlExtension = (`elem` [".yaml", ".yml"]) . takeExtension

loadJsonOrYaml :: Aeson.FromJSON a => FilePath -> IO (Either String a)
loadJsonOrYaml path
    | yamlExtension path = first show <$> decodeFileEither path
    | otherwise          = Aeson.eitherDecodeFileStrict' path
