{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
module Data.Aeson.Extended
    ( module Data.Aeson
    , Multiple (..)
    , encodePretty
    ) where

import           Control.Applicative      ((<|>))
import           Data.Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy     as BL

newtype Multiple a = Multiple {unMultiple :: [a]}

instance FromJSON a => FromJSON (Multiple a) where
    parseJSON val =
        (Multiple <$> parseJSON val) <|>
        (Multiple . return <$> parseJSON val)

encodePretty :: ToJSON a => a -> BL.ByteString
encodePretty = Pretty.encodePretty' config
  where
    config = Pretty.defConfig
        { Pretty.confIndent          = Pretty.Spaces 2
        , Pretty.confTrailingNewline = True
        , Pretty.confCompare         = Pretty.compare
        }
