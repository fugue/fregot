{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

Conversion of values to and from JSON.
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Eval.Json
    ( toValue
    , fromValue
    ) where

import           Control.Arrow       ((>>>))
import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet        as HS
import qualified Data.Vector         as V
import qualified Fregot.Eval.Number  as Number
import           Fregot.Eval.Value
import           Fregot.PrettyPrint  ((<+>))
import qualified Fregot.PrettyPrint  as PP

toValue :: A.Value -> Value
toValue = Value . \case
    A.Null     -> NullV
    A.String t -> StringV t
    A.Bool b   -> BoolV b
    A.Number s -> NumberV $! Number.fromScientific s
    A.Array a  -> ArrayV (fmap toValue a)
    A.Object o -> ObjectV $!
        HMS.fromList [(Value (StringV k), toValue v) | (k, v) <- HMS.toList o]

fromValue :: Value -> Either PP.SemDoc A.Value
fromValue = unValue >>> \case
    NullV     -> return A.Null
    StringV s -> return $! A.String s
    BoolV b   -> return $! A.Bool b
    NumberV n -> return $! A.Number (Number.toScientific n)
    ArrayV xs -> A.Array <$> traverse fromValue xs
    SetV  xs  -> A.Array <$> traverse fromValue (V.fromList $ HS.toList xs)
    ObjectV o ->
        A.Object . HMS.fromList <$> traverse fromKeyValue (HMS.toList o)
  where
    fromKeyValue (k, v) = do
        kv <- fromValue k
        vv <- fromValue v
        case kv of
            A.String t -> return (t, vv)
            _          -> Left $
                "Could not use" <+> PP.pretty (describeJsonValue kv) <+>
                "as object key in JSON, only strings are supported."

describeJsonValue :: A.Value -> String
describeJsonValue = \case
    A.Null     -> "null"
    A.String _ -> "string"
    A.Bool   _ -> "bool"
    A.Number _ -> "number"
    A.Array  _ -> "array"
    A.Object _ -> "object"
