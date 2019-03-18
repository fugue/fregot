module Data.Aeson.Extended
    ( module Data.Aeson
    , Multiple (..)
    ) where

import           Control.Applicative ((<|>))
import           Data.Aeson

newtype Multiple a = Multiple {unMultiple :: [a]}

instance FromJSON a => FromJSON (Multiple a) where
    parseJSON val =
        (Multiple <$> parseJSON val) <|>
        (Multiple . return <$> parseJSON val)
