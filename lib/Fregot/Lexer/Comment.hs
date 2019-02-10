module Fregot.Lexer.Comment
    ( Comment (..)
    ) where

import qualified Data.Text as T

newtype Comment = Comment [T.Text]
    deriving (Eq, Show)
