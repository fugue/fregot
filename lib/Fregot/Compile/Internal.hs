{-|
Copyright   : (c) 2021 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Fregot.Compile.Internal
    ( BottomUpInfo (..)
    , CompiledRuleInfo (..), criType, criBottomUp
    , CompiledRule
    ) where

import           Control.Lens              (view)
import           Control.Lens.TH           (makeLenses)
import           Fregot.Prepare.Ast        (Rule)
import           Fregot.Prepare.BottomUp   (BottomUpInfo (..))
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Fregot.Types.Rule

data CompiledRuleInfo = CompiledRuleInfo
    { _criType     :: !RuleType
    , _criBottomUp :: !BottomUpInfo
    } deriving (Show)

$(makeLenses ''CompiledRuleInfo)

instance PP.Pretty PP.Sem CompiledRuleInfo where
    pretty (CompiledRuleInfo rt bui) = PP.pretty rt PP.<+>?
        (if bui == BottomUp then Just "bottom-up" else Nothing)

instance HasRuleType CompiledRuleInfo where
    ruleTypeOf = view criType

type CompiledRule = Rule CompiledRuleInfo SourceSpan
