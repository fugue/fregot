{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
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
    , trueish
    ) where

import           Control.Lens         (preview, review, (^.))
import           Control.Lens.TH      (makePrisms)
import           Data.Hashable        (Hashable (..))
import qualified Data.HashMap.Strict  as HMS
import qualified Data.HashSet         as HS
import qualified Data.Text            as T
import qualified Data.Vector.Extended as V
import           Fregot.Eval.Number   (Number)
import qualified Fregot.Eval.Number   as Number
import           Fregot.Names         (InstVar (..))
import           Fregot.PrettyPrint   ((<+>))
import qualified Fregot.PrettyPrint   as PP
import           Fregot.Sugar         (PackageName, Var)
import qualified Fregot.Sugar         as Sugar
import           GHC.Generics         (Generic)
import qualified Text.Printf.Extended as Pf

-- | Please not that the ordering of these constructors is not arbitrary, it is
-- intended to match the sorting order that OPA implements.
data Value
    = NullV
    | BoolV                  !Bool
    | NumberV                !Number
    | StringV {-# UNPACK #-} !T.Text
    | ArrayV  {-# UNPACK #-} !(V.Vector Value)
    | SetV                   !(HS.HashSet Value)
    | ObjectV                !(HMS.HashMap Value Value)
    | FreeV   {-# UNPACK #-} !InstVar
    | WildcardV
    -- | Packages are definitely not first-class values but we can pretend that
    -- they are.
    | PackageV !PackageName
    deriving (Eq, Generic, Ord, Show)

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

instance Pf.PrintfArg Value where
    formatArg (StringV t) fmt =
        Pf.formatString (T.unpack t) fmt {Pf.fmtChar = 's'}

    formatArg (NumberV n) fmt
            | Pf.fmtChar fmt `elem` ("cdoxXbu" :: String)
            , Just int <- preview Number.int n =
        Pf.formatInt int fmt

    formatArg (NumberV n) fmt =
        Pf.formatRealFloat (n ^. Number.double) fmt

    formatArg (BoolV b) fmt | Pf.fmtChar (Pf.vFmt 't' fmt) == 't' =
        Pf.formatString (if b then "true" else "false") fmt

    formatArg _ fmt = Pf.errorBadFormat $ Pf.fmtChar fmt

-- | Is a value true in literals?
trueish :: Value -> Bool
trueish (BoolV False) = False
trueish _             = True
