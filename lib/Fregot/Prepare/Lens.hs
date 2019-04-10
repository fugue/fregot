{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fregot.Prepare.Lens
    ( ruleBodyTerms
    , literalTerms
    , termAnn
    , termVars
    , termCosmosVars
    , termCosmosNoClosures
    , termCosmosClosures
    , termRuleBodies
    , termCosmosCalls
    ) where

import           Control.Lens        (Fold, Lens', Traversal', lens, to,
                                      traverseOf, (^.))
import           Control.Lens.Plated (Plated (..), cosmos, cosmosOnOf)
import           Control.Lens.TH     (makePrisms)
import           Fregot.Prepare.Ast

$(makePrisms ''Term)

termAnn :: Lens' (Term a) a
termAnn = lens getAnn setAnn
  where
    getAnn = \case
        RefT        a _ _   -> a
        CallT       a _ _   -> a
        VarT        a _     -> a
        ScalarT     a _     -> a
        ArrayT      a _     -> a
        SetT        a _     -> a
        ObjectT     a _     -> a
        ArrayCompT  a _ _   -> a
        SetCompT    a _ _   -> a
        ObjectCompT a _ _ _ -> a

    setAnn t a = case t of
        RefT        _ x k   -> RefT        a x k
        CallT       _ f as  -> CallT       a f as
        VarT        _ v     -> VarT        a v
        ScalarT     _ s     -> ScalarT     a s
        ArrayT      _ l     -> ArrayT      a l
        SetT        _ s     -> SetT        a s
        ObjectT     _ o     -> ObjectT     a o
        ArrayCompT  _ x b   -> ArrayCompT  a x b
        SetCompT    _ x b   -> SetCompT    a x b
        ObjectCompT _ k x b -> ObjectCompT a k x b

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
        VarT        a v     -> pure (VarT a v)
        ScalarT     a s     -> pure (ScalarT a s)
        ArrayT      a xs    -> ArrayT a <$> traverse f xs
        SetT        a xs    -> SetT a <$> traverse f xs
        ObjectT     a xs    -> ObjectT a <$>
                                traverse (\(k, v) -> (,) <$> f k <*> f v) xs
        ArrayCompT  a h b   -> ArrayCompT a <$> f h <*> ruleBodyTerms f b
        SetCompT    a h b   -> SetCompT a <$> f h <*> ruleBodyTerms f b
        ObjectCompT a k v b -> ObjectCompT a <$>
                                f k <*> f v <*> ruleBodyTerms f b

-- | Fold over the direct vars of a term.
termVars :: Traversal' (Term a) (a, Var)
termVars = _VarT

termCosmosVars :: Fold (Term a) (a, Var)
termCosmosVars = cosmos . termVars

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
