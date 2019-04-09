{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Prepare.Vars
    ( Safe (..)
    , Arities
    , ovRuleBody
    , ovLiteral
    ) where

import           Control.Lens          ((^.))
import           Data.Hashable         (Hashable)
import qualified Data.HashSet.Extended as HS
import           Data.List             (foldl')
import           Data.Maybe            (fromMaybe)
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens

newtype Safe v = Safe {unSafe :: HS.HashSet v}
    deriving (Eq, Monoid, Semigroup, Show)

markSafe :: (Eq v, Hashable v) => v -> Safe v
markSafe = Safe . HS.singleton

markTermSafe :: Term a -> Safe Var
markTermSafe t =
    Safe (HS.toHashSetOf (termCosmosNoClosures . termVars . traverse) t)

isSafe :: (Eq v, Hashable v) => v -> Safe v -> Bool
isSafe v (Safe s) = HS.member v s

type Arities = Function -> Maybe Int

ovRuleBody :: Arities -> Safe Var -> RuleBody a -> Safe Var
ovRuleBody arities safe@(Safe initial) body =
    Safe $ total `HS.difference` initial
  where
    Safe total = foldl'
        (\acc lit -> let ov = ovLiteral arities acc lit in acc <> ov)
        safe
        body

ovLiteral :: Arities -> Safe Var -> Literal a -> Safe Var
ovLiteral arities safe lit
    | lit ^. literalNegation = mempty
    | otherwise              =
        ovStatement arities safe (lit ^. literalStatement)

ovStatement :: Arities -> Safe Var -> Statement a -> Safe Var
ovStatement arities safe = \case
    TermS t       -> ovTerm arities safe t
    AssignS _ v t -> markSafe v <> ovTerm arities safe t
    UnifyS _ x y  -> ovUnify arities safe x y

ovTerm :: Arities -> Safe Var -> Term a -> Safe Var
ovTerm arities safe (RefT _ x k) =
    ovTerm arities safe x <>
    markTermSafe k

ovTerm arities safe (CallT _ function args) =
    let arity = fromMaybe 0 (arities function) in
    foldMap (ovTerm arities safe) (take arity args) <>
    foldMap markTermSafe (drop arity args)

ovTerm _arities _safe (VarT _ _) = mempty

ovTerm _arities _safe (ScalarT _ _) = mempty

ovTerm arities safe (ArrayT _ xs) = foldMap (ovTerm arities safe) xs

ovTerm arities safe (SetT _ xs) = foldMap (ovTerm arities safe) xs

ovTerm arities safe (ObjectT _ xs) =
    foldMap (\(k, v) -> ovTerm arities safe k <> ovTerm arities safe v) xs

ovTerm _arities _safe (ArrayCompT _ _ _) = mempty
ovTerm _arities _safe (SetCompT _ _ _) = mempty
ovTerm _arities _safe (ObjectCompT _ _ _ _) = mempty

ovUnify :: Arities -> Safe Var -> Term a -> Term a -> Safe Var
ovUnify _arities safe (VarT _ alpha) (VarT _ beta)
    | isSafe alpha safe = markSafe beta
    | isSafe beta  safe = markSafe alpha
    | otherwise         = mempty  -- TODO(jaspervdj): Mark as unknown.

ovUnify arities safe (VarT _ alpha) y = markSafe alpha <> ovTerm arities safe y
ovUnify arities safe x (VarT _ beta)  = markSafe beta <> ovTerm arities safe x

ovUnify arities safe x y =
    ovTerm arities safe x <> ovTerm arities safe y
