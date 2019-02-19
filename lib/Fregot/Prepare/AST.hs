{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Prepare.AST
    ( RuleKind (..)
    , Rule (..), ruleName, ruleAnn, ruleDefault, ruleKind, ruleDefs
    , RuleDefinition (..), ruleDefName, ruleDefImports, ruleDefAnn, ruleArgs
    , ruleIndex, ruleValue, ruleBody

    , Sugar.PackageName (..)
    , Sugar.packageNameToString, Sugar.packageNameToText

    , Sugar.Import (..), Sugar.importAnn, Sugar.importPackage, Sugar.importAs

    , Sugar.Var (..)
    , Sugar.varToString, Sugar.varToText

    , Sugar.RuleBody
    , Sugar.Literal (..), Sugar.literalNegation, Sugar.literalExpr
    , Sugar.literalWith

    , Sugar.Expr (..), Sugar.exprAnn
    , Sugar.Term (..), Sugar.termAnn
    , Sugar.RefArg (..)
    , Sugar.Scalar (..)
    , Sugar.Object
    , Sugar.ObjectKey (..)
    , Sugar.BinOp (..)
    , Sugar.With (..), Sugar.withWith, Sugar.withAs
    ) where

import           Control.Lens.TH           (makeLenses)
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Fregot.Sugar              (Var (..))
import qualified Fregot.Sugar              as Sugar

data RuleKind
    = CompleteRule
    | GenSetRule
    | GenObjectRule
    | FunctionRule
    deriving (Eq, Show)

data Rule a = Rule
    { _ruleName    :: !Var
    , _ruleAnn     :: !SourceSpan
    , _ruleDefault :: !(Maybe (Sugar.Term a))
    , _ruleKind    :: !RuleKind
    , _ruleDefs    :: [RuleDefinition a]
    } deriving (Show)

data RuleDefinition a = RuleDefinition
    { _ruleDefName    :: !Var
    , _ruleDefImports :: ![Sugar.Import a]
    , _ruleDefAnn     :: !a
    , _ruleArgs       :: !(Maybe [Sugar.Term a])
    , _ruleIndex      :: !(Maybe (Sugar.Term a))
    , _ruleValue      :: !(Maybe (Sugar.Term a))
    , _ruleBody       :: !(Sugar.RuleBody a)
    } deriving (Show)

$(makeLenses ''Rule)
$(makeLenses ''RuleDefinition)
