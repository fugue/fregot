{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
module Fregot.Prepare.Order
    ( OrderPredicate (..)
    , reorder

    , Unsafe (..)
    , orderForClosures
    ) where

import           Control.Lens                ((^.))
import           Data.Bifunctor              (second)
import qualified Data.HashMap.Strict         as HMS
import qualified Data.HashSet.Extended       as HS
import           Data.List.NonEmpty.Extended (NonEmpty)
import qualified Data.List.NonEmpty.Extended as NonEmpty
import qualified Data.Map                    as Map
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens
import           Fregot.Prepare.Vars

data OrderPredicate e
    = OrderOk
    | OrderError e

-- | General strategy for reordering.
reorder
    :: ([a] -> a -> OrderPredicate e)  -- ^ May be added now?
    -> [a]                             -- ^ Items to order.
    -> ([a], [e])                      -- ^ Reordered, errors.
reorder orderPredicate = \items ->
    -- Associate a number with each item.
    let itemMap = Map.fromList $ zip [0 :: Int ..] items in
    loop [] Map.empty itemMap
  where
    loop ordered0 errorMap0 itemMap0 =
        -- Perform a fold over the remaining items to add them all to the list.
        let (ordered1, errorMap1) = Map.foldlWithKey'
                (\(acc, errors) k item ->
                    case orderPredicate acc item of
                        OrderOk      -> (acc ++ [item], Map.delete k errors)
                        OrderError e -> (acc, Map.insert k e errors))
                (ordered0, errorMap0)
                itemMap0

            -- Update the remaining items, these are only the ones that still
            -- have errors.
            itemMap1 = itemMap0 `Map.intersection` errorMap1 in

        if  | Map.null itemMap1 ->
                -- No items remaining means that we are done.  `errorMap1`
                -- should be empty.
                (ordered1, map snd $ Map.toList errorMap1)
            | Map.size itemMap0 == Map.size itemMap1 ->
                -- If the size of the remaining items did not change, we are
                -- stuck with the errors we have now.
                (ordered1, map snd $ Map.toList errorMap1)
            | otherwise ->
                -- Run again.
                loop ordered1 errorMap1 itemMap1

newtype Unsafe v a = Unsafe (HMS.HashMap v (NonEmpty a))
    deriving (Eq, Monoid, Semigroup, Show)

orderForClosures
    :: Arities -> Safe Var -> RuleBody a -> (RuleBody a, Unsafe Var a)
orderForClosures arities safe body = second mconcat $ reorder
    (\reordered lit ->
        -- Variables appearing in closures in this statement.
        let inClosureVars = HS.toHashSetOf
                (literalTerms . termCosmosClosures . termCosmosVars)
                lit

            -- Variabels that are both in the body as well as in the closures
            -- must be bound first.
            needOutVars =
                (bodyVars `HS.intersection` inClosureVars) `HS.difference`
                unSafe safe

            -- The current output variables.
            Safe currentOutVars = ovRuleBody arities safe reordered

            -- Missing output variables.
            missing = needOutVars `HS.difference` currentOutVars

            -- Compute unsafe variables.  Should be empty in the best case.
            unsafes =
                const (NonEmpty.singleton (lit ^. literalAnn)) <$>
                HS.toMap missing in

        if HMS.null unsafes then OrderOk else OrderError (Unsafe unsafes))
    body
  where
    -- Variables appearing in the body.
    bodyVars =
        HS.toHashSetOf (ruleBodyTerms . termCosmosNoClosures . termVars) body
