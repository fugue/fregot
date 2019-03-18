module Data.Text.Extended
    ( module Data.Text
    , fromString
    ) where

import           Control.Lens (Iso', iso)
import           Data.Text

fromString :: Iso' String Text
fromString = iso pack unpack
