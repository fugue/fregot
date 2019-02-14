{-# LANGUAGE TemplateHaskell #-}
module Fregot.Interpreter.Package
    ( Package (..), packageName
    ) where

import           Control.Lens.TH (makeLenses)
import           Fregot.Sugar

data Package = Package
    { _packageName :: !PackageName
    } deriving (Show)

$(makeLenses ''Package)
