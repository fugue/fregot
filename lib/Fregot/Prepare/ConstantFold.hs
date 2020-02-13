module Fregot.Prepare.ConstantFold
    ( rewriteRule
    ) where

import           Control.Lens        (over, (^?), _2)
import           Control.Lens.Plated (transform)
import           Control.Monad       (forM)
import qualified Data.HashSet        as HS
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector         as V
import qualified Fregot.Eval.Number  as Number
import           Fregot.Eval.Value   (Value (..), ValueF (..))
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens

rewriteRule :: Rule i a -> Rule i a
rewriteRule = over ruleTerms (transform rewriteTerm)

rewriteTerm :: Term a -> Term a
rewriteTerm term = case term of
    ScalarT a s -> case s of
        -- TODO(jaspervdj): Copy of `evalScalar`.
        String t -> ValueT a $ Value $ StringV t
        Number n -> ValueT a $ Value $ NumberV $ Number.fromScientific n
        Bool   b -> ValueT a $ Value $ BoolV   b
        Null     -> ValueT a $ Value $ NullV

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
