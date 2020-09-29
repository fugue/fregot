{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

Prototype for ordering statements in faster way (rather than the insertion sort
we have now).
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Data.SafeVar
    ( Statement (..), statementExpr, statementIn, statementOut

    , Key
    , Node (..)
    , Graph
    , toGraph
    , orderGraph
    ) where

import           Control.Lens    (foldOf, (^.))
import           Control.Lens.TH (makeLenses)
import           Data.Bifunctor  (second)
import           Data.Hashable   (Hashable)
import qualified Data.HashSet    as HS
import qualified Data.List       as List
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe      (fromMaybe, mapMaybe)

data Statement s v = Statement
    { _statementExpr :: !s
    , _statementIn   :: !(HS.HashSet v)
    , _statementOut  :: !(HS.HashSet v)
    } deriving (Eq, Show)

$(makeLenses ''Statement)

type Key = Int

data Node s v
    = StatementNode (Statement s v)
    | VarNode v
    deriving (Show)

type Graph s v = Map Key (Node s v, [Key])

toGraph
    :: forall s v. (Eq v, Hashable v, Ord v)
    => [Statement s v] -> Graph s v
toGraph statements =
    Map.mapWithKey (\k x -> (x, fromMaybe [] (Map.lookup k edges))) nodes
  where
    allVariables :: HS.HashSet v
    allVariables =
        foldOf (traverse . statementIn) statements <>
        foldOf (traverse . statementOut) statements

    idxVariables :: Map v Key
    idxVariables = Map.fromList $
        zip (HS.toList allVariables) [length idxStatements ..]

    idxStatements :: [(Key, Statement s v)]
    idxStatements = zip [0 ..] statements

    toKeyList :: HS.HashSet v -> [Key]
    toKeyList = mapMaybe (\v -> Map.lookup v idxVariables) . HS.toList

    nodes :: Map Key (Node s v)
    nodes = Map.fromList $
        [(key, VarNode var) | (var, key) <- Map.toList idxVariables] ++
        map (second StatementNode) idxStatements

    edges :: Map Key [Key]
    edges = Map.fromListWith (++) $ do
        (k, s) <- idxStatements
        let oute = toKeyList (s ^. statementOut)
            ine  = toKeyList (s ^. statementIn)
        (k, ine) : [(o, [k]) | o <- oute]

delete :: Key -> Graph s v -> Graph s v
delete = Map.delete

neighbours :: Key -> Graph s v -> [Key]
neighbours k graph = case Map.lookup k graph of
    Nothing       -> []
    Just (_, nbs) -> List.sort $ filter (`Map.member` graph) nbs

-- | Consumes a graph and orders the statements in a way that all variables are
-- assigned before they are used.  If there are no assignments for a given
-- variable, it will be returned as second part of the tuple.
orderGraph
    :: forall s v. (Eq v, Hashable v, Ord v)
    => Graph s v -> ([Statement s v], [v])
orderGraph = \graph -> go [] HS.empty [] graph
  where
    go  :: [Statement s v]  -- ^ Statement accumulator.
        -> HS.HashSet v     -- ^ Assigned variables.
        -> [v]              -- ^ Free variables.
        -> Graph s v        -- ^ Graph to consume.
        -> ([Statement s v], [v])
    -- We use 'minViewWithKey' so we can start with the first statement, which
    -- matches what the user wrote.
    go acc assigned free graph0 = case Map.minViewWithKey graph0 of
        Nothing -> (reverse acc, free)
        Just ((k, _), _) ->
            let (rk, rn) = root k HS.empty graph0
                graph1   = delete rk graph0 in
            case rn of
                VarNode f
                    | HS.member f assigned -> go acc assigned free graph1
                    | otherwise            -> go acc assigned (f : free) graph1
                StatementNode s -> go
                    (s : acc)
                    (HS.union (s ^. statementOut) assigned)
                    free
                    graph1

    -- Follow pointers to the root of a node.
    root :: Key -> HS.HashSet Key -> Graph s v -> (Key, Node s v)
    root k visited graph =
        let next = filter (not . (`HS.member` visited)) (neighbours k graph) in
        case next of
            []      -> let (node, _) = graph Map.! k in (k, node)
            (n : _) -> root n (HS.insert k visited) graph
