{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Eval.Value
    ( InstVar (..)
    , Value (..)
    , describeValue
    ) where

import           Data.Hashable        (Hashable (..))
import qualified Data.HashSet         as HS
import qualified Data.Scientific      as Scientific
import qualified Data.Text            as T
import qualified Data.Vector.Extended as V
import qualified Fregot.PrettyPrint   as PP
import           Fregot.Sugar
import           GHC.Generics         (Generic)

-- | An instantiated variable.  These have a unique (within the evaluation
-- context) number identifying them.  The var is just there for debugging
-- purposes.
data InstVar = InstVar Int Var

instance Show InstVar where
    show (InstVar n v) = T.unpack (unVar v) ++ "_" ++ show n

instance Eq InstVar where
    InstVar x _ == InstVar y _ = x == y

instance Ord InstVar where
    compare (InstVar x _) (InstVar y _) = compare x y

instance Hashable InstVar where
    hashWithSalt salt (InstVar n _) = hashWithSalt salt n

instance PP.Pretty a InstVar where
    pretty = PP.pretty . show

data Value
    = FreeV   !InstVar
    | WildcardV
    | StringV !T.Text
    -- TODO(jaspervdj): This would be cleaner using `IntV` and `DoubleV`.
    | NumberV !Scientific.Scientific
    | BoolV   !Bool
    | ArrayV  !(V.Vector Value)
    | SetV    !(HS.HashSet Value)
    -- TODO(jaspervdj): Low-hanging optimization fruit here.
    | ObjectV !(V.Vector (Value, Value))
    | NullV
    deriving (Eq, Generic, Show)

instance Hashable Value

instance PP.Pretty PP.Sem Value where
    pretty (FreeV   v) = PP.pretty v
    pretty WildcardV   = "_"
    pretty (StringV t) = PP.literal $ PP.pretty $ show $ T.unpack t
    pretty (NumberV s) = PP.literal $ PP.pretty s
    pretty (BoolV   b) = PP.literal $ if b then "true" else "false"
    pretty (ArrayV  a) = PP.array (V.toList a)
    pretty (SetV    s) = PP.set (HS.toList s)
    pretty (ObjectV o) = PP.object [(k, v) | (k, v) <- V.toList o]
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
