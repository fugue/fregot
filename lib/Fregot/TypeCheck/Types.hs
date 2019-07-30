{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Fregot.TypeCheck.Types
    ( Type (..)
    ) where

import           Fregot.PrettyPrint ((<+>))
import qualified Fregot.PrettyPrint as PP

data Type
    = Any
    | Or Type Type
    | Number
    | String
    | Boolean
    | Null
    deriving (Eq)

instance PP.Pretty PP.Sem Type where
    pretty Any      = PP.keyword "any"
    pretty (Or x y) = PP.pretty x <+> PP.punctuation "|" <+> PP.pretty y
    pretty Number   = PP.keyword "number"
    pretty String   = PP.keyword "string"
    pretty Boolean  = PP.keyword "boolean"
    pretty Null     = PP.keyword "null"
