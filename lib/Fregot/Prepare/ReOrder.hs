{-# LANGUAGE TemplateHaskell #-}
module Fregot.Prepare.ReOrder
    (
    ) where


import           Control.Lens       ((^.))
import           Control.Lens.TH    (makeLenses)
import           Data.Hashable      (Hashable)
import qualified Data.HashSet       as HS
import           Fregot.Prepare.Ast

data Constraint v = In v | Out v deriving (Show)

-- | Set of declared variables.
data Scope v = Scope
    { _locals  :: !(HS.HashSet v)
    , _globals :: !(HS.HashSet [v])  -- Maybe a function instead?
    } deriving (Show)

$(makeLenses ''Scope)

defined :: (Eq v, Hashable v) => v -> Scope v -> Bool
defined v s = HS.member v (s ^. locals) || HS.member [v] (s ^. globals)

inferTerm :: Scope Var -> Term a -> [Constraint Var]
inferTerm scope (VarT _ v)
    | defined v scope = [Out v]
    | otherwise       = [In v]
inferTerm
