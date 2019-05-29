{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Fregot.TypeCheck.Types
    ( Type (..)
    ) where

import           Fregot.PrettyPrint ((<+>))
import qualified Fregot.PrettyPrint as PP

data Type v
    = Any
    | Or (Type v) (Type v)
    | Number
    | String
    | Boolean
    | Null
    | Var v
    deriving (Eq)

instance PP.Pretty PP.Sem v => PP.Pretty PP.Sem (Type v) where
    pretty Any      = PP.keyword "any"
    pretty (Or x y) = PP.pretty x <+> PP.punctuation "|" <+> PP.pretty y
    pretty Number   = PP.keyword "number"
    pretty String   = PP.keyword "string"
    pretty Boolean  = PP.keyword "boolean"
    pretty Null     = PP.keyword "null"
    pretty (Var v)  = PP.pretty v
