{-# LANGUAGE TemplateHaskell #-}
module Data.SafeVar
    ( Statement (..), statementExpr, statementIn, statementOut
    ) where

import           Control.Lens.TH (makeLenses)
import qualified Data.HashSet    as HS

data Statement s v = Statement
    { _statementExpr :: !s
    , _statementIn   :: !(HS.HashSet v)
    , _statementOut  :: !(HS.HashSet v)
    } deriving (Eq, Show)

$(makeLenses ''Statement)
