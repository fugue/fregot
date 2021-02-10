{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

Turn the prepared rules into a graph, based on their dependencies.
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
module Fregot.Compile.Graph
    ( ruleDependencies
    ) where

import           Control.Lens.Plated (plate)
import           Control.Lens        (to, toListOf, (^..), (^?), _2)
import           Data.Maybe          (isJust)
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens
import qualified Data.HashSet        as HS
import qualified Data.Vector         as V
import qualified Fregot.Tree         as Tree

ruleDependencies :: Tree.Tree e -> Rule ty a -> HS.HashSet Key
ruleDependencies tree0 =
    foldMap termDependencies . toListOf ruleTerms
  where
    -- | If a key references `foo`; it may access `foo.bar` in a later
    -- statement.  This function takes care of collecting all the suffixes.
    suffixes :: Key -> HS.HashSet Key
    suffixes prefix = HS.fromList $ case Tree.descendant prefix tree0 of
        Nothing -> [prefix]
        Just d  -> [prefix] <> map (prefix <>) (Tree.keys d)

    -- | Calculate the dependencies of a term.
    termDependencies :: Term a -> HS.HashSet Key
    termDependencies term = case unRefT term of
        Just (h, t) ->
            -- Given `foo.bar`, compute both the dependencies of `foo.bar` (as
            -- a ref) as well as those of `bar`.
            refDependencies h t <> foldMap termDependencies t
        Nothing -> HS.unions $
            -- Qualified names in this term (directly).
            (map suffixes $ term ^.. termNames . traverse . _QualifiedName) ++
            -- And recurse over the childen of this term.
            (term ^.. plate . to termDependencies)

    refDependencies :: Term a -> [Term a] -> HS.HashSet Key
    refDependencies (NameT _ (QualifiedName k)) t =
        -- Compute precise dependencies.
        case Tree.descendant k tree0 of
            Nothing   -> suffixes k
            Just root -> treeRefDependencies k root t
    refDependencies h _ = termDependencies h

    treeRefDependencies :: Key -> Tree.Tree e -> [Term a] -> HS.HashSet Key
    treeRefDependencies prefix tree refs = case refs of
        -- Any rules need to report all their suffixes.
        _ | isJust (Tree.root tree) -> suffixes prefix
        [] -> suffixes prefix
        -- Granular search of a known-string package.
        h : t | Just (String s) <- h ^? termToScalar . _2 ->
            let k = Key (V.singleton (mkVar s)) in
            case Tree.descendant k tree of
                Nothing -> mempty
                Just c  -> treeRefDependencies (prefix <> k) c t
        -- Wildcard?  Recurse over all children.
        _ : t -> mconcat $ do
            (v, c) <- Tree.children tree
            let k = Key (V.singleton v)
            pure $ treeRefDependencies (prefix <> k) c t
