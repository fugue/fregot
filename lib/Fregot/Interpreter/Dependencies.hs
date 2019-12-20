-- | Pure, simple dependency management.
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Fregot.Interpreter.Dependencies
    ( DependencyError

    , Graph (..)
    , plan
    , evict
    ) where

import           Data.Hashable      (Hashable)
import qualified Data.HashSet       as HS
import qualified Fregot.PrettyPrint as PP

data DependencyError k
    = DependencyCycle [k]
    deriving (Show)

instance PP.Pretty PP.Sem k => PP.Pretty PP.Sem (DependencyError k) where
    pretty (DependencyCycle rev) =
        let trail = reverse $ zipWith ($)
                (id : repeat (PP.<+> "depends on")) (map PP.pretty rev) in
        "Cyclic dependency error:" PP.<$$> PP.ind (PP.vcat trail)

-- | Somewhat abstract graph representation.
data Graph k = Graph
    { graphDone         :: [k]
    , graphIsDone       :: k -> Bool
    , graphDependencies :: k -> [k]
    }

-- | Compute a plan to build the requested nodes.
plan
    :: forall k. (Eq k, Hashable k)
    => Graph k                         -- ^ Dependency graph
    -> HS.HashSet k                    -- ^ Wanted nodes
    -> Either (DependencyError k) [k]  -- ^ Error or plan
plan Graph {..} = go HS.empty []
  where
    go :: HS.HashSet k -> [k] -> HS.HashSet k -> Either (DependencyError k) [k]
    go done acc wanted = case HS.toList wanted of
        [] -> pure (reverse acc)
        w : _ | w `HS.member` done || graphIsDone w ->
            go done acc (w `HS.delete` wanted)
        w : _ -> do
            x <- chase done [] w
            go (x `HS.insert` done) (x : acc) (x `HS.delete` wanted)

    chase :: HS.HashSet k -> [k] -> k -> Either (DependencyError k) k
    chase done trail current =
        -- All dependencies not yet taken care of.
        let next =
                [ n
                | n <- graphDependencies current
                , not (n `HS.member` done)
                , not (graphIsDone n)
                ] in

        -- Look where we should head next.
        case next of
            []                       -> Right current
            (n : _) | n `elem` trail -> Left $ DependencyCycle (n : current : trail)
            n : _                    -> chase done (current : trail) n

-- | If a node is removed from the "done" set, we'll also need to remove all the
-- nodes that depended on it.  This function computes that set in a simple way
-- by just computing the fixpoint.
evict
    :: (Eq k, Hashable k)
    => Graph k          -- ^ Dependency graph
    -> HS.HashSet k     -- ^ Nodes you want to evict
    -> HS.HashSet k     -- ^ All nodes that need to be evicted
evict Graph {..} = go
  where
    go closure =
        let new   = HS.fromList $ filter ood graphDone
            ood n =
                not (n `HS.member` closure) &&
                any (`HS.member` closure) (graphDependencies n) in

        if HS.null new then closure else go (closure `HS.union` new)
