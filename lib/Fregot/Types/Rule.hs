{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Fregot.Types.Rule
    ( RuleType (..), _CompleteRuleType, _GenSetRuleType, _GenObjectRuleType
    , _FunctionType
    , ruleTypeToType
    ) where

import           Control.Lens.TH       (makePrisms)
import qualified Fregot.Prepare.Ast    as Ast
import qualified Fregot.PrettyPrint    as PP
import           Fregot.Types.Internal

data RuleType
    = CompleteRuleType Type
    | GenSetRuleType Type
    | GenObjectRuleType (StaticDynamic Ast.Scalar Type)
    | FunctionType Int
    | ErrorType
    deriving (Show)

instance PP.Pretty PP.Sem RuleType where
    pretty = PP.pretty . ruleTypeToType

-- | Converts a rule type to a normal type.
ruleTypeToType :: RuleType -> Type
ruleTypeToType (CompleteRuleType ty)   = ty
ruleTypeToType (GenSetRuleType ty)     = setOf ty
ruleTypeToType (GenObjectRuleType oty) = object oty
ruleTypeToType (FunctionType _)        = error "ruleTypeToType (FunctionType _)"
ruleTypeToType ErrorType               = unknown

$(makePrisms ''RuleType)
