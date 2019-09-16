{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Fregot.TypeCheck.Types
    ( Type (..)
    , RuleType (..), _CompleteRuleType, _GenSetRuleType, _GenObjectRuleType
    , _FunctionType
    ) where

import           Control.Lens.TH    (makePrisms)
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

data RuleType
    = CompleteRuleType Type
    | GenSetRuleType Type
    | GenObjectRuleType Type Type
    | FunctionType  Int  -- TODO(jaspervdj)

instance PP.Pretty PP.Sem RuleType where
    pretty = \case
        CompleteRuleType ty -> "rule" <+> PP.punctuation "=" <+> PP.pretty ty
        GenSetRuleType ty -> "rule" <+> PP.punctuation "[" <+>
            PP.pretty ty <+> PP.punctuation "]"
        GenObjectRuleType idx ty -> "rule" <+> PP.punctuation "[" <+>
            PP.pretty idx <+> PP.punctuation "]" <+> PP.punctuation "=" <+>
            PP.pretty ty
        FunctionType arity -> "rule" <+> PP.punctuation "(" <+>
            PP.commaSep (replicate arity "_") <+> PP.punctuation ")" <+>
            PP.punctuation "=" <+> "_"

$(makePrisms ''RuleType)
