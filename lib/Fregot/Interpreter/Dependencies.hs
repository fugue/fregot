-- | Pure, simple dependency management.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Fregot.Interpreter.Dependencies
    ( Graph (..), graphDone, graphDependencies
    , empty
    , plan
    ) where

import           Control.Lens    ((^.))
import           Control.Lens.TH (makeLenses)
import           Data.Hashable   (Hashable)
import qualified Data.HashSet    as HS

data DependencyError k
    = DependencyCycle [k]
    deriving (Show)

-- | Somewhat abstract graph representation.  We use a higher-order record so
-- that we actually never need to construct a graph, we can just get information
-- from the modules in the functions.
data Graph k = Graph
    { _graphDone         :: k -> Bool
    , _graphDependencies :: k -> [k]
    }

$(makeLenses ''Graph)

empty :: Graph k
empty = Graph (const True) (const [])

-- | Compute a plan to build the requested nodes.
plan
    :: forall k. (Eq k, Hashable k)
    => Graph k                         -- ^ Dependency graph
    -> HS.HashSet k                    -- ^ Wanted nodes
    -> Either (DependencyError k) [k]  -- ^ Error or plan
plan graph = go HS.empty []
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
                | n <- (graph ^. graphDependencies) current
                , not (n `HS.member` done)
                , not $ (graph ^. graphDone) n
                ] in

        -- Look where we should head next.
        case next of
            []                       -> Right current
            (n : _) | n `elem` trail -> Left $ DependencyCycle (n : trail)
            n : _                    -> chase done (current : trail) n
