{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}
module Fregot.Prepare.ReOrder
    (
    ) where

{-

import           Control.Lens         ((^.))
import           Control.Lens.TH      (makeLenses)
import           Control.Monad        (forM)
import           Control.Monad        (zipWithM)
import           Data.Functor         ((<$))
import           Data.Hashable        (Hashable)
import qualified Data.HashMap.Strict  as HMS
import qualified Data.HashSet         as HS
import qualified Fregot.Eval.Builtins as Builtins
import           Fregot.Prepare.Ast

-- NOTE(jaspervdj): This is where we would have a spot to push down and
-- propagate up types as well.
data Dir v = Down v | Up v deriving (Functor, Show)

down :: Dir ()
down = Down ()

up :: Dir ()
up = Up ()

-- | Set of declared variables.
data Scope v = Scope
    { _locals   :: !(HS.HashSet v)
    , _globals  :: !(HS.HashSet [v])  -- Maybe a function instead?
    , _builtins :: !(HMS.HashMap Function Builtins.Builtin)
    }

$(makeLenses ''Scope)

defined :: (Eq v, Hashable v) => v -> Scope v -> Bool
defined v s = HS.member v (s ^. locals) || HS.member [v] (s ^. globals)

orderTerm :: Scope Var -> Dir ty -> Term a -> [Dir Var]
orderTerm scope dir (RefT _ x k) =
    orderTerm scope dir x <>
    -- The `k` goes up, since we can have things like `list[i]` where `i` is
    -- generated.
    orderTerm scope up k

orderTerm scope dir (CallT _ function args)
    | Just builtin <- HMS.lookup function Builtins.builtins =
        let dirs = replicate (Builtins.arity builtin) down ++ repeat up in
        concat $ forM (zip dirs args) $ \(d, arg) -> orderTerm scope d arg

    | otherwise = error "TODO(jaspervdj): user-defined functions"

orderTerm scope dir (VarT _ v)
    | defined v scope = [Up v]
    | otherwise       = [v <$ dir]

orderTerm scope _ (ScalarT _ _) = []

orderTerm scope dir (ArrayT _ xs) =
    concatMap (orderTerm scope dir) xs

orderTerm scope dir (SetT _ xs) =
    concatMap (orderTerm scope dir) xs

orderTerm scope dir (ObjectT _ xs) =
    orderObject scope dir xs

orderObject :: Scope Var -> Dir ty -> Object a -> [Dir Var]
orderObject scope dir object = concat $ forM object $ \(k, t) ->
    orderTerm scope dir k <> orderTerm scope dir t

-}
