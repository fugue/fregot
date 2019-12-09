-- | Turn the prepared rules into a graph, based on their dependencies.
module Fregot.Compile.Graph
    ( ruleDependencies
    ) where

import qualified Data.HashSet.Extended as HS
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens

ruleDependencies :: Rule ty a -> HS.HashSet (PackageName, Var)
ruleDependencies = HS.toHashSetOf $
    ruleTerms . termCosmosNames . traverse . _QualifiedName
