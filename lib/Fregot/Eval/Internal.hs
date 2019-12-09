{-# LANGUAGE TemplateHaskell #-}
module Fregot.Eval.Internal
    ( Context (..), unification, locals, nextInstVar
    , emptyContext
    ) where

import           Control.Lens.TH     (makeLenses)
import qualified Data.HashMap.Strict as HMS
import           Data.Unification    (Unification)
import qualified Data.Unification    as Unification
import qualified Data.Unique         as Unique
import           Fregot.Eval.Value   (InstVar, Value)
import           Fregot.Names        (Var)

data Context = Context
    { _unification :: !(Unification InstVar Value)
    , _locals      :: !(HMS.HashMap Var InstVar)
    , _nextInstVar :: !Unique.Unique
    }

$(makeLenses ''Context)

emptyContext :: Context
emptyContext = Context
    { _unification = Unification.empty
    , _locals      = mempty
    , _nextInstVar = 0
    }
