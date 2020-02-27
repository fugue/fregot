{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fregot.Prepare.Lens
    ( ruleTerms
    , ruleDefinitionTerms

    , ruleBodyTerms
    , literalTerms
    , termAnn
    , termNames
    , termCosmosNames
    , termCosmosNoClosures
    , termCosmosClosures
    , termRuleBodies
    , termCosmosCalls

    , valueToScalar
    , termToScalar
    ) where

import           Control.Lens        (Fold, Lens', Prism', Traversal', aside,
                                      lens, prism', to, traverseOf, (^.))
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
    <*> traverseOf (traverse . ruleDefinitionTerms) f (rule ^. ruleDefs)

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
        RefT        a _ _   -> a
        CallT       a _ _   -> a
        NameT       a _     -> a
        ArrayT      a _     -> a
        SetT        a _     -> a
        ObjectT     a _     -> a
        ArrayCompT  a _ _   -> a
        SetCompT    a _ _   -> a
        ObjectCompT a _ _ _ -> a
        ValueT      a _     -> a
        ErrorT      a       -> a

    setAnn t a = case t of
        RefT        _ x k   -> RefT        a x k
        CallT       _ f as  -> CallT       a f as
        NameT       _ v     -> NameT       a v
        ArrayT      _ l     -> ArrayT      a l
        SetT        _ s     -> SetT        a s
        ObjectT     _ o     -> ObjectT     a o
        ArrayCompT  _ x b   -> ArrayCompT  a x b
        SetCompT    _ x b   -> SetCompT    a x b
        ObjectCompT _ k x b -> ObjectCompT a k x b
        ValueT      _ v     -> ValueT      a v
        ErrorT      _       -> ErrorT      a

statementTerms :: Traversal' (Statement a) (Term a)
statementTerms f = \case
    UnifyS  a x y -> UnifyS a <$> f x <*> f y
    AssignS a v x -> AssignS a v <$> f x
    TermS       x -> TermS <$> f x

literalTerms :: Traversal' (Literal a) (Term a)
literalTerms f lit = Literal (lit ^. literalAnn) (lit ^. literalNegation)
    <$> statementTerms f (lit ^. literalStatement)
    <*> traverseOf (traverse . withAs) f (lit ^. literalWith)

ruleBodyTerms :: Traversal' (RuleBody a) (Term a)
ruleBodyTerms = traverse . literalTerms

instance Plated (Term a) where
    plate f = \case
        RefT        a x k   -> RefT a <$> f x <*> f k
        CallT       a g xs  -> CallT a g <$> traverse f xs
        NameT       a v     -> pure (NameT a v)
        ArrayT      a xs    -> ArrayT a <$> traverse f xs
        SetT        a xs    -> SetT a <$> traverse f xs
        ObjectT     a xs    -> ObjectT a <$>
                                traverse (\(k, v) -> (,) <$> f k <*> f v) xs
        ArrayCompT  a h b   -> ArrayCompT a <$> f h <*> ruleBodyTerms f b
        SetCompT    a h b   -> SetCompT a <$> f h <*> ruleBodyTerms f b
        ObjectCompT a k v b -> ObjectCompT a <$>
                                f k <*> f v <*> ruleBodyTerms f b
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
termCosmosClosures :: Fold (Term a) (Term a)
termCosmosClosures = cosmos . termClosures

-- | Fold over all terms that DO NOT appear in a closure.
termCosmosNoClosures :: Fold (Term a) (Term a)
termCosmosNoClosures = cosmosOnOf noClosure (plate . noClosure)
  where
    noClosure f e = case e of
        ArrayCompT  _ _ _   -> pure e
        SetCompT    _ _ _   -> pure e
        ObjectCompT _ _ _ _ -> pure e
        _                   -> f e

-- | Fold over the direct subclosures of a term.
termClosures :: Traversal' (Term a) (Term a)
termClosures f e = case e of
    ArrayCompT  _ _ _   -> f e
    SetCompT    _ _ _   -> f e
    ObjectCompT _ _ _ _ -> f e
    _                   -> pure e

-- | Fold over the direct closures bodies of a term.
termRuleBodies :: Traversal' (Term a) (RuleBody a)
termRuleBodies f e = case e of
    ArrayCompT  a h   b -> ArrayCompT a h <$> f b
    SetCompT    a h   b -> SetCompT a h <$> f b
    ObjectCompT a k v b -> ObjectCompT a k v <$> f b
    _                   -> pure e

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
