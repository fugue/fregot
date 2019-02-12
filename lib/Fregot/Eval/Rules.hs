-- | A table of rules.
{-# LANGUAGE TemplateHaskell #-}
module Fregot.Eval.Rules
    ( Rules, table
    , empty
    , insert
    , lookup
    ) where

import           Control.Lens              ((%~), (&), (^.))
import           Control.Lens.TH           (makeLenses)
import qualified Data.HashMap.Strict       as HMS
import           Data.Maybe                (fromMaybe)
import           Fregot.Sources.SourceSpan
import           Fregot.Sugar
import           Prelude                   hiding (head, lookup)

data Rules = Rules
    { _table :: !(HMS.HashMap Var [Rule SourceSpan])
    }

$(makeLenses ''Rules)

empty :: Rules
empty = Rules HMS.empty

-- | TODO: Error out if arity is wrong.
insert :: Rule SourceSpan -> Rules -> Rules
insert rule rules = rules &
    table %~ (HMS.insertWith (++) (rule ^. head . name) [rule])

lookup :: Var -> Rules -> [Rule SourceSpan]
lookup var = fromMaybe [] . HMS.lookup var . (^. table)
