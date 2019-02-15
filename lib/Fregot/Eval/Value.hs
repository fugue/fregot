{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Eval.Value
    ( Value (..)
    , describeValue
    ) where

import qualified Data.HashMap.Strict as HMS
import qualified Data.Scientific     as Scientific
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Fregot.PrettyPrint  as PP
import           Fregot.Sugar

data Value
    = FreeV   !Var
    | WildcardV
    | StringV !T.Text
    -- TODO(jaspervdj): This would be cleaner using `IntV` and `DoubleV`.
    | NumberV !Scientific.Scientific
    | BoolV   !Bool
    | ArrayV  !(V.Vector Value)
    | SetV    !(V.Vector Value)
    | ObjectV !(HMS.HashMap T.Text Value)
    | NullV
    deriving (Eq, Show)

instance PP.Pretty PP.Sem Value where
    pretty (FreeV   v) = PP.pretty v
    pretty WildcardV   = "_"
    pretty (StringV t) = PP.literal $ PP.pretty $ show $ T.unpack t
    pretty (NumberV s) = PP.literal $ PP.pretty s
    pretty (BoolV   b) = PP.literal $ if b then "true" else "false"
    pretty (ArrayV  a) = PP.array (V.toList a)
    pretty (SetV    s) = PP.set (V.toList s)
    pretty (ObjectV o) = PP.object
        [ (StringV k, v)
        | (k, v) <- HMS.toList o
        ]
    pretty NullV       = PP.literal "null"

describeValue :: Value -> String
describeValue = \case
    FreeV   _ -> "free variable"
    WildcardV -> "wildcard"
    StringV _ -> "string"
    NumberV _ -> "number"
    BoolV   _ -> "boolean"
    ArrayV  _ -> "array"
    SetV    _ -> "set"
    ObjectV _ -> "object"
    NullV     -> "null"
