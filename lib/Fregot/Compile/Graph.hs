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
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens
import qualified Fregot.PrettyPrint  as PP
import qualified Fregot.Tree         as Tree

-- | TODO(jaspervdj): We compute this a bunch of times and this innocent-looking
-- fold traverses the whole syntax tree.  It's probably better to just store
-- this in 'Rule'.
ruleDependencies :: Tree.Tree e -> Rule ty a -> HS.HashSet Key
ruleDependencies tree =
    foldMap termDependencies . toListOf ruleTerms
  where
    termDependencies :: Term a -> HS.HashSet Key
    termDependencies term = case unRefT term of
        Just (h, t) -> refDependencies h t <> foldMap termDependencies t
        Nothing     -> foldMap suffixes $
            (term ^.. termNames . traverse . _QualifiedName) ++
            (term ^.. plate . to termDependencies . to HS.toList . traverse)

    refDependencies :: Term a -> [Term a] -> HS.HashSet Key
    refDependencies (NameT _ (QualifiedName k)) t = error $ show $
        "getting dependencies for " <> PP.pretty' k <> " and " <>
        PP.commaSep (map PP.pretty' t)
    refDependencies h _ = termDependencies h

    suffixes :: Key -> HS.HashSet Key
    suffixes prefix = HS.fromList $ case Tree.descendant prefix tree of
        Nothing -> [prefix]
        Just d  -> [prefix] <> map (prefix <>) (Tree.keys d)
