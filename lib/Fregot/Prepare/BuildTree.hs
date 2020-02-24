--------------------------------------------------------------------------------
-- | Helper module for building rule trees from JSON and YAML documents.
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Prepare.BuildTree
    ( BuildTree (..)
    , toTree
    , toTerm
    ) where

import           Control.Lens              (review)
import qualified Data.List                 as L
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens
import           Fregot.Prepare.Package    (PreparedRule)
import           Fregot.Sources.SourceSpan (SourceSpan)
import qualified Fregot.Tree               as Tree

data BuildTree
    = BuildSingleton (Term SourceSpan)
    | BuildTree      SourceSpan [(SourceSpan, Var, BuildTree)]

toTree
    :: PackageName -> [(SourceSpan, Var, BuildTree)] -> Tree.Tree PreparedRule
toTree pkgname = L.foldl' Tree.union Tree.empty . map toRuleOrTree
  where
    toRuleOrTree (loc, var, BuildSingleton t) = Tree.singleton
        (review varFromKey var)
        Rule
            { _rulePackage = pkgname
            , _ruleName    = var
            , _ruleKey     = review qualifiedVarFromKey (pkgname, var)
            , _ruleAnn     = loc
            , _ruleKind    = CompleteRule
            , _ruleInfo    = ()
            , _ruleDefault = Nothing
            , _ruleDefs    = pure RuleDefinition
                { _ruleDefName    = var
                , _ruleDefImports = mempty
                , _ruleDefAnn     = loc
                , _ruleArgs       = Nothing
                , _ruleIndex      = Nothing
                , _ruleValue      = Just t
                , _ruleBodies     = []
                , _ruleElses      = []
                }
            }

    toRuleOrTree (_, var, BuildTree _ t) =
        Tree.parent [(var, toTree (pkgname <> mkPackageName [unVar var]) t)]

-- NOTE(jaspervdj): Should we have this use the new 'ValueT' to only represent
-- values?
toTerm :: BuildTree -> Term SourceSpan
toTerm (BuildSingleton t) = t
toTerm (BuildTree loc children) = ObjectT loc $
    [ (review termToScalar (l, String $ unVar v), toTerm child)
    | (l, v, child) <- children
    ]
