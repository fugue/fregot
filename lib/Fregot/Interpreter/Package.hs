{-# LANGUAGE TemplateHaskell #-}
module Fregot.Interpreter.Package
    ( Package (..), packageName, packageRules
    , CompiledRule
    , RuleDefinition (..), ruleDefImports, ruleDefRule

    , empty
    , insert
    , lookup
    ) where

import           Control.Lens              ((%~), (&), (^.))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad.Parachute
import qualified Data.HashMap.Strict       as HMS
import           Data.Maybe                (fromMaybe)
import           Fregot.Error              (Error)
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Fregot.Sugar
import           Prelude                   hiding (lookup)

data Package = Package
    { _packageName  :: !PackageName
    , _packageRules :: !(HMS.HashMap Var CompiledRule)
    } deriving (Show)

type CompiledRule = [RuleDefinition]

data RuleDefinition = RuleDefinition
    { _ruleDefImports :: ![Import SourceSpan]
    , _ruleDefRule    :: !(Rule SourceSpan)
    } deriving (Show)

$(makeLenses ''Package)
$(makeLenses ''RuleDefinition)

empty :: PackageName -> Package
empty name = Package
    { _packageName  = name
    , _packageRules = HMS.empty
    }

-- | Add a new rule.
insert
    :: Monad m
    => [Import SourceSpan]
    -> Rule SourceSpan
    -> Package
    -> ParachuteT Error m Package
insert imports rule package = return $
    -- | TODO(jaspervdj): Error out if arity is wrong.
    package & packageRules %~ HMS.insertWith (++) rname [rdef]
  where
    rname = rule ^. ruleHead . ruleName
    rdef  = RuleDefinition imports rule

lookup :: Var -> Package -> [RuleDefinition]
lookup var pkg = fromMaybe [] $ HMS.lookup var (pkg ^. packageRules)
