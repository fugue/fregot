-- | Turn the prepared rules into a graph, based on their dependencies.
{-# LANGUAGE Rank2Types #-}
module Fregot.Compile.Graph
    ( ruleDependencies
    ) where

import           Control.Lens        (Fold, to)
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens
import qualified Fregot.Tree         as Tree

-- | TODO(jaspervdj): We compute this a bunch of times and this innocent-looking
-- fold traverses the whole syntax tree.  It's probably better to just store
-- this in 'Rule'.
ruleDependencies :: Tree.Tree e -> Fold (Rule ty a) Key
ruleDependencies tree =
    ruleTerms . termCosmosNames . traverse . _QualifiedName .
    to suffixes . traverse
  where
    suffixes :: Key -> [Key]
    suffixes prefix = case Tree.descendant prefix tree of
        Nothing -> [prefix]
        Just d  -> [prefix] <> map (prefix <>) (Tree.keys d)
