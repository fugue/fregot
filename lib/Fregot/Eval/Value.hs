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
    , Value (..), _FreeV, _WildcardV, _StringV, _NumberV, _BoolV, _ArrayV, _SetV
    , _ObjectV, _NullV, _PackageV
    , describeValue
    , emptyObject
    , updateObject
    ) where

import           Control.Lens         (review)
import           Control.Lens.TH      (makePrisms)
import           Data.Hashable        (Hashable (..))
import qualified Data.HashMap.Strict  as HMS
import qualified Data.HashSet         as HS
import qualified Data.Text            as T
import qualified Data.Vector.Extended as V
import           Fregot.Eval.Number   (Number)
import           Fregot.PrettyPrint   ((<+>))
import qualified Fregot.PrettyPrint   as PP
import           Fregot.Sugar         (PackageName, Var)
import qualified Fregot.Sugar         as Sugar
import           GHC.Generics         (Generic)

-- | An instantiated variable.  These have a unique (within the evaluation
-- context) number identifying them.  The var is just there for debugging
-- purposes.
data InstVar = InstVar {-# UNPACK #-} !Int {-# UNPACK #-} !Var

instance Show InstVar where
    show (InstVar n v) = T.unpack (Sugar.unVar v) ++ "_" ++ show n

instance Eq InstVar where
    InstVar x _ == InstVar y _ = x == y

instance Ord InstVar where
    compare (InstVar x _) (InstVar y _) = compare x y

instance Hashable InstVar where
    hashWithSalt salt (InstVar n _) = hashWithSalt salt n

instance PP.Pretty a InstVar where
    pretty = PP.pretty . show

data Value
    = FreeV   {-# UNPACK #-} !InstVar
    | WildcardV
    | StringV {-# UNPACK #-} !T.Text
    | NumberV                !Number
    | BoolV                  !Bool
    | ArrayV  {-# UNPACK #-} !(V.Vector Value)
    | SetV                   !(HS.HashSet Value)
    | ObjectV                !(HMS.HashMap Value Value)
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
    pretty (NumberV n)  = PP.literal $ PP.pretty n
    pretty (BoolV   b)  = PP.literal $ if b then "true" else "false"
    pretty (ArrayV  a)  = PP.array (V.toList a)
    pretty (SetV    s)  = PP.set (HS.toList s)
    pretty (ObjectV o)  = PP.object [(k, v) | (k, v) <- HMS.toList o]
    pretty NullV        = PP.literal "null"
    pretty (PackageV p) = PP.keyword "package" <+> PP.pretty p

$(makePrisms ''Value)

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
    PackageV p -> "package " ++ review Sugar.packageNameFromString p

emptyObject :: Value
emptyObject = ObjectV HMS.empty

-- | Updates a path in the object.  This is mainly used to implement the `with`
-- modifier.
updateObject :: [Var] -> Value -> Value -> Maybe Value
updateObject []       insertee _           = Just insertee
updateObject (v : vs) insertee (ObjectV o) =
    case HMS.lookup k o of
        Nothing -> do
            nest <- updateObject vs insertee emptyObject
            return $ ObjectV $ HMS.insert k nest o
        Just val -> do
            nest <- updateObject vs insertee val
            return $ ObjectV $ HMS.insert k nest o
  where
    k = StringV (Sugar.unVar v)

-- TODO(jaspervdj): Better error
updateObject _ _ _ = Nothing
