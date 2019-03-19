-- | Conversion of values to and from JSON.
module Fregot.Eval.Json
    ( toValue
    ) where

import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as HMS
import qualified Data.Scientific     as Scientific
import           Fregot.Eval.Value

toValue :: A.Value -> Value
toValue A.Null       = NullV
toValue (A.String t) = StringV t
toValue (A.Bool b)   = BoolV b
toValue (A.Number s) = either DoubleV IntV $ Scientific.floatingOrInteger s
toValue (A.Array a)  = ArrayV (fmap toValue a)
toValue (A.Object o) = ObjectV $!
    HMS.fromList [(StringV k, toValue v) | (k, v) <- HMS.toList o]
