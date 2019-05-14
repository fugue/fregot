{-# LANGUAGE LambdaCase #-}
module Fregot.Names.Renamer
    ( RenamerEnv (..)
    , RenamerM
    , renameModule
    , renameExpr
    ) where

import           Control.Lens              ((^.))
import           Control.Monad.Parachute   (ParachuteT)
import           Control.Monad.Reader      (Reader)
import           Fregot.Error              (Error)
import           Fregot.Names
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Fregot.Sugar

data RenamerEnv = RenamerEnv

type RenamerM a = ParachuteT Error (Reader RenamerEnv) a

type Rename f = f SourceSpan Var -> RenamerM (f SourceSpan Name)

renameModule :: Rename Module
renameModule modul = Module
    <$> pure (modul ^. modulePackage)
    <*> pure (modul ^. moduleImports)
    <*> traverse renameRule (modul ^. modulePolicy)

renameRule :: Rename Rule
renameRule rule = Rule
    <$> renameRuleHead (rule ^. ruleHead)
    <*> traverse renameRuleBody (rule ^. ruleBodies)
    <*> traverse renameRuleElse (rule ^. ruleElses)

renameRuleHead :: Rename RuleHead
renameRuleHead rh = RuleHead
    <$> pure (rh ^. ruleAnn)
    <*> pure (rh ^. ruleDefault)
    <*> pure (rh ^. ruleName)
    <*> traverse (traverse renameTerm) (rh ^. ruleArgs)
    <*> traverse renameTerm (rh ^. ruleIndex)
    <*> traverse renameTerm (rh ^. ruleValue)

renameRuleBody :: RuleBody SourceSpan Var -> RenamerM (RuleBody SourceSpan Name)
renameRuleBody = traverse renameLiteral

renameRuleElse :: Rename RuleElse
renameRuleElse re = RuleElse
    <$> pure (re ^. ruleElseAnn)
    <*> traverse renameTerm (re ^. ruleElseValue)
    <*> renameRuleBody (re ^. ruleElseBody)

renameLiteral :: Rename Literal
renameLiteral lit = Literal
    <$> pure (lit ^. literalAnn)
    <*> pure (lit ^. literalNegation)
    <*> renameExpr (lit ^. literalExpr)
    <*> traverse renameWith (lit ^. literalWith)

renameExpr :: Rename Expr
renameExpr = \case
    TermE a t      -> TermE   a <$> renameTerm t
    BinOpE a x b y -> BinOpE  a <$> renameExpr x <*> pure b <*> renameExpr y
    ParensE a x    -> ParensE a <$> renameExpr x

renameTerm :: Rename Term
renameTerm = \case
    RefT _ _ _ _ -> error "TODO: actual work"
    CallT _ _ _ -> error "TODO: actual work"

    -- TODO(jaspervdj): Find out of the variable is a rule in this package.  If
    -- so, it's a QualifiedName, if not it's a LocalName.
    VarT a v -> pure (VarT a (LocalName v))

    ScalarT a s -> pure (ScalarT a s)

    ArrayT  a xs -> ArrayT  a <$> traverse renameExpr xs
    SetT    a xs -> SetT    a <$> traverse renameExpr xs
    ObjectT a xs -> ObjectT a <$> renameObject xs

    ArrayCompT  a   x b -> ArrayCompT  a <$> renameTerm x <*> renameRuleBody b
    SetCompT    a   x b -> SetCompT    a <$> renameTerm x <*> renameRuleBody b
    ObjectCompT a k x b -> ObjectCompT a <$>
        renameObjectKey k <*> renameTerm x <*> renameRuleBody b

renameObject :: Object SourceSpan Var -> RenamerM (Object SourceSpan Name)
renameObject = traverse renameItem
  where
    renameItem (k, v) = (,) <$> renameObjectKey k <*> renameExpr v

renameObjectKey :: Rename ObjectKey
renameObjectKey = \case
    ScalarK a s -> pure (ScalarK a s)
    -- TODO(jaspervdj): Can we have a rule name here?
    VarK a uv -> pure (VarK a uv)
    RefK _ _ _ -> error "TODO: actual work"

renameWith :: Rename With
renameWith with = With
    <$> pure (with ^. withAnn)
    <*> pure (with ^. withWith)
    <*> renameTerm (with ^. withAs)
