{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fregot.Prepare.Lens
    ( ruleTerms
    , ruleDefinitionBodies
    , ruleDefinitionTerms

    , ruleBodyTerms
    , comprehensionBody
    , comprehensionTerms
    , literalTerms
    , termAnn
    , termNames
    , termCosmosNames
    , termCosmosNoClosures
    , termCosmosClosures
    , termRuleBodies
    , termCosmosCalls
    , termToClosure

    , valueToScalar
    , termToScalar
    ) where

import           Control.Lens        (Fold, Lens', Prism', Traversal', aside,
                                      lens, prism', to, traverseOf, (^.), _2)
import           Control.Lens.Plated (Plated (..), cosmos, cosmosOnOf)
import           Fregot.Eval.Number  as Number
import           Fregot.Eval.Value   (Value (..), ValueF (..))
import           Fregot.Names
import           Fregot.Prepare.Ast

-- All direct terms of the rule, combine with 'cosmos' to traverse deeper.
ruleTerms :: Traversal' (Rule i a) (Term a)
ruleTerms f rule = Rule
    <$> pure (rule ^. rulePackage)
    <*> pure (rule ^. ruleName)
    <*> pure (rule ^. ruleKey)
    <*> pure (rule ^. ruleAnn)
    <*> pure (rule ^. ruleKind)
    <*> pure (rule ^. ruleInfo)
    <*> traverse f (rule ^. ruleDefault)
    <*> pure (rule ^. ruleAssign)
    <*> traverseOf (traverse . ruleDefinitionTerms) f (rule ^. ruleDefs)

-- All rule "bodies" inside a rule.  Useful for optimizations.  Does not include
-- nested comprehensions.
ruleDefinitionBodies :: Traversal' (RuleDefinition a) (RuleBody a)
ruleDefinitionBodies f rdef = RuleDefinition
    <$> pure (rdef ^. ruleDefName)
    <*> pure (rdef ^. ruleDefImports)
    <*> pure (rdef ^. ruleDefAnn)
    <*> pure (rdef ^. ruleArgs)
    <*> pure (rdef ^. ruleIndex)
    <*> pure (rdef ^. ruleValue)
    <*> traverseOf traverse f (rdef ^. ruleBodies)
    <*> traverseOf (traverse . ruleElseBody) f (rdef ^. ruleElses)

-- | All direct terms of a rule definition, combine with 'cosmos' to traverse
-- deeper.
ruleDefinitionTerms :: Traversal' (RuleDefinition a) (Term a)
ruleDefinitionTerms f rdef = RuleDefinition
    <$> pure (rdef ^. ruleDefName)
    <*> pure (rdef ^. ruleDefImports)
    <*> pure (rdef ^. ruleDefAnn)
    <*> traverseOf (traverse . traverse) f (rdef ^. ruleArgs)
    <*> traverse f (rdef ^. ruleIndex)
    <*> traverse f (rdef ^. ruleValue)
    <*> traverseOf (traverse . ruleBodyTerms) f (rdef ^. ruleBodies)
    <*> traverseOf (traverse . ruleElseTerms) f (rdef ^. ruleElses)

-- | All direct terms of a rule else construct, combine with 'cosmos' to
-- traverse deeper.
ruleElseTerms :: Traversal' (RuleElse a) (Term a)
ruleElseTerms f relse = RuleElse
    <$> pure (relse ^. ruleElseAnn)
    <*> traverse f (relse ^. ruleElseValue)
    <*> traverseOf ruleBodyTerms f (relse ^. ruleElseBody)

termAnn :: Lens' (Term a) a
termAnn = lens getAnn setAnn
  where
    getAnn = \case
        RefT        a _ _ -> a
        CallT       a _ _ -> a
        NameT       a _   -> a
        ArrayT      a _   -> a
        SetT        a _   -> a
        ObjectT     a _   -> a
        CompT       a _   -> a
        ValueT      a _   -> a
        ErrorT      a     -> a

    setAnn t a = case t of
        RefT        _ x k  -> RefT        a x k
        CallT       _ f as -> CallT       a f as
        NameT       _ v    -> NameT       a v
        ArrayT      _ l    -> ArrayT      a l
        SetT        _ s    -> SetT        a s
        ObjectT     _ o    -> ObjectT     a o
        CompT       _ c    -> CompT a c
        ValueT      _ v    -> ValueT      a v
        ErrorT      _      -> ErrorT      a

statementTerms :: Traversal' (Statement a) (Term a)
statementTerms f = \case
    UnifyS  a x y -> UnifyS a <$> f x <*> f y
    AssignS a x y -> AssignS a <$> f x <*> f y
    TermS       x -> TermS <$> f x

    IndexedCompS a (IndexedComprehension u ks v c) ->
        IndexedCompS a . IndexedComprehension u ks v <$>
        traverseOf comprehensionTerms f c

literalTerms :: Traversal' (Literal a) (Term a)
literalTerms f lit = Literal (lit ^. literalAnn) (lit ^. literalNegation)
    <$> statementTerms f (lit ^. literalStatement)
    <*> traverseOf (traverse . withAs) f (lit ^. literalWith)

ruleBodyTerms :: Traversal' (RuleBody a) (Term a)
ruleBodyTerms = traverse . literalTerms

comprehensionTerms :: Traversal' (Comprehension a) (Term a)
comprehensionTerms f = \case
    ArrayComp  h b   -> ArrayComp <$> f h <*> ruleBodyTerms f b
    SetComp    h b   -> SetComp <$> f h <*> ruleBodyTerms f b
    ObjectComp k v b -> ObjectComp <$> f k <*> f v <*> ruleBodyTerms f b

instance Plated (Term a) where
    plate f = \case
        RefT        a x k   -> RefT a <$> f x <*> f k
        CallT       a g xs  -> CallT a g <$> traverse f xs
        NameT       a v     -> pure (NameT a v)
        ArrayT      a xs    -> ArrayT a <$> traverse f xs
        SetT        a xs    -> SetT a <$> traverse f xs
        ObjectT     a xs    -> ObjectT a <$>
                                traverse (\(k, v) -> (,) <$> f k <*> f v) xs
        CompT       a c     -> CompT a <$> traverseOf comprehensionTerms f c
        ValueT      a v     -> pure (ValueT a v)
        ErrorT      a       -> pure (ErrorT a)

-- | Fold over the direct names of a term.
termNames :: Traversal' (Term a) (a, Name)
termNames f e = case e of
    NameT a n -> uncurry NameT <$> f (a, n)
    CallT a (NamedFunction n) args ->
        (\(a', n') -> CallT a' (NamedFunction n') args) <$> f (a, n)
    _ -> pure e

termCosmosNames :: Fold (Term a) (a, Name)
termCosmosNames = cosmos . termNames

-- | Fold over all closures in a term recursively.
termCosmosClosures :: Fold (Term a) (Comprehension a)
termCosmosClosures = cosmos . termToClosure . _2

-- | Fold over all terms that DO NOT appear in a closure.
termCosmosNoClosures :: Fold (Term a) (Term a)
termCosmosNoClosures = cosmosOnOf noClosure (plate . noClosure)
  where
    noClosure f e = case e of
        CompT _ _ -> pure e
        _         -> f e

-- | Selects a term if it is a closure.
termToClosure :: Prism' (Term a) (a, Comprehension a)
termToClosure = _CompT

comprehensionBody :: Lens' (Comprehension a) (RuleBody a)
comprehensionBody = lens
    (\case
        ArrayComp  _ b   -> b
        SetComp    _ b   -> b
        ObjectComp _ _ b -> b)
   (\comp b -> case comp of
        ArrayComp  h   _ -> ArrayComp  h b
        SetComp    h   _ -> SetComp    h b
        ObjectComp k v _ -> ObjectComp k v b)

-- | Fold over the direct closures bodies of a term.
termRuleBodies :: Traversal' (Term a) (RuleBody a)
termRuleBodies = termToClosure . _2 . comprehensionBody

-- | Find referenced functions in a term.
termCosmosCalls :: Fold (Term a) (a, Function)
termCosmosCalls = cosmos . _CallT . to (\(ann, f, _) -> (ann, f))

valueToScalar :: Prism' Value Scalar
valueToScalar = prism' fromScalar toScalar
  where
    fromScalar = \case
        String t -> Value $ StringV t
        Number n -> Value . NumberV $ Number.fromScientific n
        Bool   b -> Value $ BoolV b
        Null     -> Value NullV

    toScalar (Value v) = case v of
        StringV t -> Just $ String t
        NumberV n -> Just . Number $ Number.toScientific n
        BoolV   b -> Just $ Bool b
        NullV     -> Just Null
        _         -> Nothing

termToScalar :: Prism' (Term a) (a, Scalar)
termToScalar = _ValueT . aside valueToScalar
