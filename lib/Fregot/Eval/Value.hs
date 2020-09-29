{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
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
    , ValueF (..), _StringV, _NumberV, _BoolV, _ArrayV, _SetV, _ObjectV, _NullV
    , Value (..)
    , describeValue
    , describeValueF
    , emptyObject
    , trueish
    , true
    , string
    ) where

import           Control.Lens         (preview, (^.))
import           Control.Lens.TH      (makePrisms)
import           Data.Hashable        (Hashable (..))
import qualified Data.HashMap.Strict  as HMS
import qualified Data.HashSet         as HS
import qualified Data.Text            as T
import qualified Data.Vector.Extended as V
import           Fregot.Eval.Number   (Number)
import qualified Fregot.Eval.Number   as Number
import           Fregot.Names         (InstVar (..))
import qualified Fregot.PrettyPrint   as PP
import           GHC.Generics         (Generic)
import qualified Text.Printf.Extended as Pf

-- | Please not that the ordering of these constructors is not arbitrary, it is
-- intended to match the sorting order that OPA implements.
data ValueF a
    = NullV
    | BoolV                  !Bool
    | NumberV                !Number
    | StringV {-# UNPACK #-} !T.Text
    | ArrayV  {-# UNPACK #-} !(V.Vector a)
    -- Note how set and object cannot contain free variables, so we don't use
    -- 'a' here but just 'Value'.
    | SetV                   !(HS.HashSet Value)
    | ObjectV                !(HMS.HashMap Value a)
    deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance Hashable a => Hashable (ValueF a)

instance PP.Pretty PP.Sem a => PP.Pretty PP.Sem (ValueF a) where
    pretty (StringV t) = PP.literal $ PP.pretty $ show $ T.unpack t
    pretty (NumberV n) = PP.literal $ PP.pretty n
    pretty (BoolV   b) = PP.literal $ if b then "true" else "false"
    pretty (ArrayV  a) = PP.array (V.toList a)
    pretty (SetV    s) = PP.set (HS.toList s)
    pretty (ObjectV o) = PP.object [(k, v) | (k, v) <- HMS.toList o]
    pretty NullV       = PP.literal "null"

newtype Value = Value {unValue :: ValueF Value}
    deriving (Eq, Generic, Hashable, Ord, PP.Pretty PP.Sem, Show)

$(makePrisms ''ValueF)

describeValue :: Value -> String
describeValue = describeValueF . unValue

describeValueF :: ValueF a -> String
describeValueF = \case
    StringV  _ -> "string"
    NumberV  _ -> "number"
    BoolV    _ -> "boolean"
    ArrayV   _ -> "array"
    SetV     _ -> "set"
    ObjectV  _ -> "object"
    NullV      -> "null"

emptyObject :: Value
emptyObject = Value (ObjectV HMS.empty)

instance Pf.PrintfArg Value where
    -- Strings are always rendered as strings.
    formatArg (Value (StringV t)) fmt =
        Pf.formatString (T.unpack t) fmt {Pf.fmtChar = 's'}

    -- Whole numbers.
    formatArg (Value (NumberV n)) fmt
            | Pf.fmtChar fmt `elem` ("cdoxXbu" :: String)
            , Just int <- preview Number.int n =
        Pf.formatInt int fmt

    -- Floating numbers.
    formatArg (Value (NumberV n)) fmt =
        Pf.formatRealFloat (n ^. Number.double) fmt

    -- Booleans.
    formatArg (Value (BoolV b)) fmt | Pf.fmtChar (Pf.vFmt 't' fmt) == 't' =
        Pf.formatString (if b then "true" else "false") fmt

    -- For other user-defined types, records, sets, strings, etc., the Pretty
    -- instance is used.
    formatArg value fmt | Pf.fmtChar fmt == 'v' = Pf.formatString
        (PP.display . PP.removeLines . PP.renderDefaults $ PP.pretty' value)
        fmt {Pf.fmtChar = 's', Pf.fmtPrecision = Nothing}

    formatArg _ fmt = Pf.errorBadFormat $ Pf.fmtChar fmt

-- | Is a value true in literals?
trueish :: Value -> Bool
trueish (Value (BoolV False)) = False
trueish _                     = True

true :: Value
true = Value (BoolV True)

string :: T.Text -> Value
string = Value . StringV
