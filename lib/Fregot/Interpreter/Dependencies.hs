-- | Pure, simple dependency management.
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Fregot.Interpreter.Dependencies
    ( Graph (..)
    , plan
    , evict
    ) where

import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet        as HS

data DependencyError k
    = DependencyCycle [k]
    deriving (Show)

-- | Somewhat abstract graph representation.
data Graph k where
    Graph :: HMS.HashMap k a -> (k -> [k]) -> Graph k

-- | Compute a plan to build the requested nodes.
plan
    :: forall k. (Eq k, Hashable k)
    => Graph k                         -- ^ Dependency graph
    -> HS.HashSet k                    -- ^ Wanted nodes
    -> Either (DependencyError k) [k]  -- ^ Error or plan
plan (Graph gdone gdeps) = go HS.empty []
  where
    go :: HS.HashSet k -> [k] -> HS.HashSet k -> Either (DependencyError k) [k]
    go done acc wanted = case HS.toList wanted of
        []    -> pure (reverse acc)
        w : _ -> do
            x <- chase done [] w
            go (x `HS.insert` done) (x : acc) (x `HS.delete` wanted)

    chase :: HS.HashSet k -> [k] -> k -> Either (DependencyError k) k
    chase done trail current =
        -- All dependencies not yet taken care of.
        let next =
                [ n
                | n <- gdeps current
                , not (n `HS.member` done)
                , not (n `HMS.member` gdone)
                ] in

        -- Look where we should head next.
        case next of
            []                       -> Right current
            (n : _) | n `elem` trail -> Left $ DependencyCycle (n : trail)
            n : _                    -> chase done (current : trail) n

-- | If a node is removed from the "done" set, we'll also need to remove all the
-- nodes that depended on it.  This function computes that set in a simple way
-- by just computing the fixpoint.
evict
    :: (Eq k, Hashable k)
    => Graph k          -- ^ Dependency graph
    -> HS.HashSet k     -- ^ Nodes you want to evict
    -> HS.HashSet k     -- ^ All nodes that need to be evicted
evict (Graph gdone gdeps) = go
  where
    go closure =
        let new     = HS.fromMap $ () <$ HMS.filterWithKey ood gdone
            ood n _ =
                not (n `HS.member` closure) &&
                any (`HS.member` closure) (gdeps n) in

        if HS.null new then closure else go (closure `HS.union` new)
