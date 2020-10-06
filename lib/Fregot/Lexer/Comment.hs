{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
module Fregot.Lexer.Comment
    ( Comment (..)
    ) where

import qualified Data.Text as T

newtype Comment = Comment [T.Text]
    deriving (Eq, Show)
