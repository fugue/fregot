{-|
Copyright   : (c) 2021 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE LambdaCase #-}
module Fregot.Prepare.BottomUp
    ( rewriteRule
    ) where

import           Control.Lens              (allOf, anyOf, (&), (.~), (^.), _2)
import           Data.Maybe                (isNothing)
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens
import           Fregot.Sources.SourceSpan (SourceSpan)

rewriteRule :: Rule i SourceSpan -> Rule i SourceSpan
rewriteRule rule = rule & ruleBottomUp .~ bottomUpRule rule

bottomUpRule :: Rule i a -> Bool
bottomUpRule rule =
    (rule ^. ruleKind == GenObjectRule || rule ^. ruleKind == GenSetRule) &&
    allOf (ruleDefs . traverse) bottomUpRuleDefinition rule

bottomUpRuleDefinition :: RuleDefinition a -> Bool
bottomUpRuleDefinition rdef =
    bottomUpVar (rdef ^. ruleIndex) && bottomUpVar (rdef ^. ruleValue) &&
    null (rdef ^. ruleElses) && isNothing (rdef ^. ruleArgs)
  where
    bottomUpVar Nothing = True
    bottomUpVar (Just (NameT _ (LocalName var))) =
        allOf (ruleBodies . traverse) ((== Yes) . assignedOnceRuleBody var) rdef
    bottomUpVar (Just _) = False

data AssignedOnce = Yes | No | Unknown deriving (Eq)

instance Semigroup AssignedOnce where
    Yes     <> Yes     = No
    No      <>  _      = No
    _       <> No      = No
    x       <> Unknown = x
    Unknown <> y       = y

instance Monoid AssignedOnce where
    mempty = Unknown

assignedOnceRuleBody :: Var -> RuleBody a -> AssignedOnce
assignedOnceRuleBody var = foldMap (assignedOnceLiteral var)

assignedOnceLiteral :: Var -> Literal a -> AssignedOnce
assignedOnceLiteral var lit
    | appearsInWith = No
    | otherwise     = assignedOnceStatement var $ lit ^. literalStatement
  where
    appearsInWith =
        anyOf (literalWith . traverse . withAs) (appearsInTerm var) lit

assignedOnceStatement :: Var -> Statement a -> AssignedOnce
assignedOnceStatement var = \case
    UnifyS _ (NameT _ (LocalName v)) _ | v == var -> Yes
    UnifyS _ _ (NameT _ (LocalName v)) | v == var -> Yes
    UnifyS _ l r ->
        if appearsInTerm var l || appearsInTerm var r then No else Unknown
    AssignS _ (NameT _ (LocalName v)) _ | v == var -> Yes
    AssignS _ _ r ->
        if appearsInTerm var r then No else Unknown
    TermS t -> if appearsInTerm var t then No else Unknown
    IndexedCompS _ _ -> No

appearsInTerm :: Var -> Term a -> Bool
appearsInTerm v term = anyOf (termCosmosNames . _2 . _LocalName) (== v) term
