{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

Graph-related builtins.
-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Fregot.Builtins.Graph
    ( builtins
    ) where

import           Control.Lens             (ifoldMap)
import qualified Data.HashMap.Strict      as HMS
import qualified Data.HashSet             as HS
import qualified Data.Vector              as V
import           Fregot.Builtins.Internal
import           Fregot.Eval.Value
import           Fregot.Names
import           Fregot.Types.Builtins    ((🡒))
import qualified Fregot.Types.Builtins    as Ty
import           Fregot.Types.Internal    ((∪))
import qualified Fregot.Types.Internal    as Ty

builtins :: Builtins IO
builtins = HMS.fromList
    [ (NamedFunction (QualifiedName "graph.reachable"), builtin_graph_reachable)
    , (NamedFunction (BuiltinName   "walk"),            builtin_walk)
    ]

builtin_graph_reachable :: Monad m => Builtin m
builtin_graph_reachable = Builtin
    (Ty.objectOf Ty.any (Ty.setOf Ty.any ∪ Ty.arrayOf Ty.any) 🡒
        Ty.setOf Ty.any ∪ Ty.arrayOf Ty.any 🡒
        Ty.out (Ty.setOf Ty.unknown)) $ pure $
    \(Cons (Value graph) (Cons initial Nil)) -> pure $

    -- Find neighbours of the vertex, Nothing if the vertex is not there.
    let neighbours :: Value -> Maybe [Value]
        neighbours x = case graph of
            ObjectV o -> case HMS.lookup x o of
                Just (Value (ArrayV v)) -> Just $ V.toList v
                Just (Value (SetV s))   -> Just $ HS.toList s
                _                       -> Nothing
            _ -> Nothing

        -- Explore the queue until empty.
        walk :: HS.HashSet Value -> [Value] -> HS.HashSet Value
        walk reached [] = reached
        walk reached (q : queue) = case neighbours q of
            -- Not a member of the graph, ignore it.
            Nothing -> walk reached queue
            -- Update queue and reached set.
            Just nbs -> walk (HS.insert q reached) $
                filter (not . (`HS.member` reached)) nbs ++ queue in

    Value . SetV . walk HS.empty $! case initial of
        InL list -> list
        InR set  -> HS.toList set

builtin_walk :: Monad m => Builtin m
builtin_walk = Builtin
    -- TODO(jaspervdj): We could type this way better if we had proper "pair"
    -- array types.
    (Ty.any 🡒 Ty.out (Ty.arrayOf Ty.unknown)) $ pure $
    \(Cons val Nil) -> walk V.empty val
  where
    walk path val =
        (pure $ Value $ ArrayV [Value (ArrayV path), val]) <>
        (case unValue val of
            ArrayV v  -> ifoldMap (\i -> walk (path <> [toVal i])) v
            SetV   s  -> foldMap (\v -> walk (path <> [v]) v) s
            ObjectV o -> ifoldMap (\k -> walk (path <> [toVal k])) o
            _         -> mempty)
