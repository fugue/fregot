-- | Turn the prepared rules into a graph, based on their dependencies.
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
module Fregot.Compile.Graph
    ( ruleDependencies
    ) where

import           Control.Lens        (to, toListOf, (^..))
import           Control.Lens.Plated (plate)
import qualified Data.HashSet        as HS
import qualified Data.Vector         as V
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens
import qualified Fregot.Tree         as Tree

-- | TODO(jaspervdj): We compute this a bunch of times and this innocent-looking
-- fold traverses the whole syntax tree.  It's probably better to just store
-- this in 'Rule'.
ruleDependencies :: Tree.Tree e -> Rule ty a -> HS.HashSet Key
ruleDependencies tree0 =
    foldMap termDependencies . toListOf ruleTerms
  where
    termDependencies :: Term a -> HS.HashSet Key
    termDependencies term = case unRefT term of
        Just (h, t) -> refDependencies h t <> foldMap termDependencies t
        Nothing     -> HS.unions $
            (map suffixes $ term ^.. termNames . traverse . _QualifiedName) ++
            (term ^.. plate . to termDependencies)

    suffixes :: Key -> HS.HashSet Key
    suffixes prefix = HS.fromList $ case Tree.descendant prefix tree0 of
        Nothing -> [prefix]
        Just d  -> [prefix] <> map (prefix <>) (Tree.keys d)

    refDependencies :: Term a -> [Term a] -> HS.HashSet Key
    refDependencies (NameT _ (QualifiedName k)) t =
        case Tree.descendant k tree0 of
            Nothing   -> HS.singleton k
            Just root -> treeRefDependencies k root t
    refDependencies h _ = termDependencies h

    treeRefDependencies :: Key -> Tree.Tree e -> [Term a] -> HS.HashSet Key
    treeRefDependencies prefix tree refs =
        (case Tree.root tree of
            Just _  -> HS.singleton prefix
            Nothing -> mempty) <>
        (case refs of
            [] -> HS.fromList $ map (prefix <>) (Tree.keys tree)
            ScalarT _ (String s) : t ->
                let k = Key (V.singleton (mkVar s)) in
                case Tree.descendant k tree of
                    Nothing -> mempty
                    Just c  -> treeRefDependencies (prefix <> k) c t
            _ : t -> mconcat $ do
                (v, c) <- Tree.children tree
                let k = Key (V.singleton v)
                pure $ treeRefDependencies (prefix <> k) c t)
