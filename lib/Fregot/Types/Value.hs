-- | Inferring already-evaluated values.
module Fregot.Types.Value
    ( inferValue
    ) where

import           Control.Lens          (review)
import           Data.Either           (partitionEithers)
import qualified Data.HashMap.Strict   as HMS
import qualified Data.HashSet          as HS
import qualified Data.Vector           as V
import qualified Fregot.Eval.Number    as Number
import qualified Fregot.Eval.Value     as V
import qualified Fregot.Prepare.Ast    as Ast
import           Fregot.Types.Internal
import           Prelude               hiding (null)

inferValue :: V.Value -> Type
inferValue (V.FreeV _)     = unknown
inferValue V.WildcardV     = unknown
inferValue (V.PackageV _)  = unknown
inferValue (V.StringV _)   = string
inferValue (V.NumberV _)   = number
inferValue (V.BoolV _)     = boolean
inferValue V.NullV         = null
inferValue (V.ArrayV arr)  = arrayOf $ unions $ map inferValue $ V.toList arr
inferValue (V.SetV set)    = setOf $ unions $ map inferValue $ HS.toList set
inferValue (V.ObjectV obj) =
    let (static, dynamic) = partitionEithers $ do
            (k, v) <- HMS.toList obj
            pure $ case valueToScalar k of
                Just s  -> Left (s, inferValue v)
                Nothing -> Right (inferValue k, inferValue v) in
    review singleton $ Object $ Obj
        (HMS.fromList static)
        (case dynamic of
            []    -> Nothing
            _ : _ -> Just (unions (map fst dynamic), unions (map snd dynamic)))

valueToScalar :: V.Value -> Maybe Ast.Scalar
valueToScalar V.NullV       = Just Ast.Null
valueToScalar (V.BoolV b)   = Just $ Ast.Bool b
valueToScalar (V.StringV s) = Just $ Ast.String s
valueToScalar (V.NumberV n) = Just $ Ast.Number $ Number.toScientific n
valueToScalar _             = Nothing
