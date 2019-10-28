{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Fregot.Types.Rule
    ( RuleType (..), _CompleteRuleType, _GenSetRuleType, _GenObjectRuleType
    , _FunctionType
    , ruleTypeToType
    ) where

import           Control.Lens.TH       (makePrisms)
import qualified Fregot.PrettyPrint    as PP
import           Fregot.Types.Internal

data RuleType
    = CompleteRuleType Type
    | GenSetRuleType Type
    | GenObjectRuleType (ObjectType Type)
    | FunctionType  Int  -- TODO(jaspervdj)

instance PP.Pretty PP.Sem RuleType where
    pretty = PP.pretty . ruleTypeToType

-- | Converts a rule type to a normal type.
ruleTypeToType :: RuleType -> Type
ruleTypeToType (CompleteRuleType ty)   = ty
ruleTypeToType (GenSetRuleType ty)     = Set ty
ruleTypeToType (GenObjectRuleType oty) = Object oty
ruleTypeToType (FunctionType _)        = error "ruleTypeToType (FunctionType _)"

$(makePrisms ''RuleType)
