{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
module Data.Text.Extended
    ( module Data.Text
    , fromString
    ) where

import           Control.Lens (Iso', iso)
import           Data.Text

fromString :: Iso' String Text
fromString = iso pack unpack
