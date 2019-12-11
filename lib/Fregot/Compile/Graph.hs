-- | Turn the prepared rules into a graph, based on their dependencies.
module Fregot.Compile.Graph
    ( ruleDependencies
    ) where

import           Control.Lens        (Fold)
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens

-- | TODO(jaspervdj): We compute this a bunch of times and this innocent-looking
-- fold traverses the whole syntax tree.  It's probably better to just store
-- this in 'Rule'.
ruleDependencies :: Fold (Rule ty a) Key
ruleDependencies = ruleTerms . termCosmosNames . traverse . _QualifiedName
