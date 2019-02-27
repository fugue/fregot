{-# LANGUAGE BangPatterns               #-}
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
    , emptyObject
    , updateObject
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
    -- | Packages are definitely not first-class values but we can pretend that
    -- they are.
    | PackageV !PackageName
    deriving (Eq, Generic, Show)

instance Hashable Value

instance PP.Pretty PP.Sem Value where
    pretty (FreeV   v)  = PP.pretty v
    pretty WildcardV    = "_"
    pretty (StringV t)  = PP.literal $ PP.pretty $ show $ T.unpack t
    pretty (NumberV s)  = PP.literal $ PP.pretty s
    pretty (BoolV   b)  = PP.literal $ if b then "true" else "false"
    pretty (ArrayV  a)  = PP.array (V.toList a)
    pretty (SetV    s)  = PP.set (HS.toList s)
    pretty (ObjectV o)  = PP.object [(k, v) | (k, v) <- V.toList o]
    pretty NullV        = PP.literal "null"
    pretty (PackageV p) = PP.pretty p

describeValue :: Value -> String
describeValue = \case
    FreeV   v  -> "free variable (" ++ show v ++ ")"
    WildcardV  -> "wildcard"
    StringV  _ -> "string"
    NumberV  _ -> "number"
    BoolV    _ -> "boolean"
    ArrayV   _ -> "array"
    SetV     _ -> "set"
    ObjectV  _ -> "object"
    NullV      -> "null"
    PackageV p -> "package " ++ packageNameToString p

emptyObject :: Value
emptyObject = ObjectV V.empty

-- | Updates a path in the object.  This is mainly used to implement the `with`
-- modifier.
updateObject :: [Var] -> Value -> Value -> Maybe Value
updateObject []       insertee _           = Just insertee
updateObject (v : vs) insertee (ObjectV o) =
    case V.findIndex ((== k) . fst) o of
        Nothing -> do
            nest <- updateObject vs insertee emptyObject
            return $ ObjectV $ V.cons (k, nest) o
        Just idx -> do
            let (_, val) = o V.! idx
            nest <- updateObject vs insertee val
            return $ ObjectV $ o V.// [(idx, (k, nest))]
  where
    k = StringV (unVar v)

-- TODO(jaspervdj): Better error
updateObject _ _ _ = Nothing
