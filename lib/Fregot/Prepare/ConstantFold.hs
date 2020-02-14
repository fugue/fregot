module Fregot.Prepare.ConstantFold
    ( rewriteRule
    ) where

import           Control.Lens        (over, review, (^?), _2)
import           Control.Lens.Plated (transform)
import           Control.Monad       (forM)
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet        as HS
import qualified Data.Vector         as V
import           Fregot.Eval.Value   (Value (..), ValueF (..))
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens

rewriteRule :: Rule i a -> Rule i a
rewriteRule = over ruleTerms (transform rewriteTerm)

rewriteTerm :: Term a -> Term a
rewriteTerm term = case term of
    ScalarT a s -> ValueT a $ review valueToScalar s

    ArrayT a arr -> maybe term
        (ValueT a . Value . ArrayV . V.fromList)
        (mapM (^? _ValueT . _2) arr)

    SetT a set -> maybe term
        (ValueT a . Value . SetV . HS.fromList)
        (mapM (^? _ValueT . _2) set)

    ObjectT a obj -> maybe term
        (ValueT a . Value . ObjectV . HMS.fromList)
        (forM obj $ \(k, v) ->
            (,) <$> (k ^? _ValueT . _2) <*> (v ^? _ValueT . _2))

    _ -> term
