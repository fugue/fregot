{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Lexer.Position
    ( Position (..), line, column
    , initPosition
    , getPosition
    ) where

import           Control.Lens.TH        (makeLenses)
import qualified Data.Aeson.TH.Extended as Aeson
import           Data.Binary            (Binary)
import           Data.Data              (Data)
import           Data.Typeable          (Typeable)
import           GHC.Generics           (Generic)
import qualified Text.Parsec            as Parsec

data Position = Position
    { _line   :: {-# UNPACK #-} !Int
    , _column :: {-# UNPACK #-} !Int
    } deriving (Eq, Data, Generic, Ord, Show, Typeable)

instance Binary Position

initPosition :: Position
initPosition = Position 1 1

getPosition
    :: Monad m => Parsec.ParsecT s u m Position
getPosition = do
    sourcePos <- Parsec.getPosition
    return $! Position
        { _line   = Parsec.sourceLine sourcePos
        , _column = Parsec.sourceColumn sourcePos
        }

$(makeLenses ''Position)
$(Aeson.deriveJSON Aeson.fregotOptions ''Position)
