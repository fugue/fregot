{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Fregot.Compile.Order
    ( OrderPredicate (..)
    , reorder

    , Safe (..)
    , Unsafe (..)
    , orderForClosures
    , orderForSafety
    , orderTermForSafety
    ) where

import           Control.Lens                (traverseOf, view, (^.))
import           Control.Monad.Extended      (mapAccumM)
import           Control.Monad.Identity      (runIdentity)
import           Control.Monad.Parachute     (runParachuteT)
import           Control.Monad.Writer        (Writer, runWriter, writer)
import           Data.Foldable               (for_)
import           Data.Functor                (($>))
import qualified Data.HashMap.Strict         as HMS
import qualified Data.HashSet.Extended       as HS
import           Data.List.NonEmpty.Extended (NonEmpty)
import qualified Data.List.NonEmpty.Extended as NonEmpty
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromMaybe, listToMaybe)
import qualified Data.Unification            as Unification
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens
import qualified Fregot.PrettyPrint          as PP
import           Fregot.Sources.SourceSpan   (SourceSpan)
import qualified Fregot.Types.Infer          as Types
import qualified Fregot.Types.Internal       as Types

data OrderPredicate a e
    = OrderOk a
    | OrderError e

-- | General strategy for reordering.
reorder
    :: (b -> [a] -> a -> OrderPredicate b e)  -- ^ May be added now?
    -> b                                      -- ^ Initial accumulator.
    -> [a]                                    -- ^ Items to order.
    -> (b, [a], [e])                             -- ^ Reordered, errors.
reorder orderPredicate = \userAcc items ->
    -- Associate a number with each item.
    let itemMap = Map.fromList $ zip [0 :: Int ..] items in
    loop userAcc [] Map.empty itemMap
  where
    loop userAcc0 ordered0 errorMap0 itemMap0 =
        -- Perform a fold over the remaining items to add them all to the list.
        let (userAcc1, ordered1, errorMap1) = Map.foldlWithKey'
                (\(userAcc, acc, errors) k item ->
                    case orderPredicate userAcc acc item of
                        OrderOk ua   -> (ua, acc ++ [item], Map.delete k errors)
                        OrderError e -> (userAcc, acc, Map.insert k e errors))
                (userAcc0, ordered0, errorMap0)
                itemMap0

            -- Update the remaining items, these are only the ones that still
            -- have errors.
            itemMap1 = itemMap0 `Map.intersection` errorMap1 in

        if  | Map.null itemMap1 ->
                -- No items remaining means that we are done.  `errorMap1`
                -- should be empty.
                (userAcc1, ordered1, map snd $ Map.toList errorMap1)
            | Map.size itemMap0 == Map.size itemMap1 ->
                -- If the size of the remaining items did not change, we are
                -- stuck with the errors we have now.
                (userAcc1, ordered1, map snd $ Map.toList errorMap1)
            | otherwise ->
                -- Run again.
                loop userAcc1 ordered1 errorMap1 itemMap1

newtype Safe v = Safe {unSafe :: HS.HashSet v}
    deriving (Eq, Monoid, Semigroup, Show)

instance PP.Pretty PP.Sem v => PP.Pretty PP.Sem (Safe v) where
    pretty = PP.set . HS.toList . unSafe

newtype Unsafe v a = Unsafe {unUnsafe :: HMS.HashMap v (NonEmpty a)}
    deriving (Eq, Monoid, Semigroup, Show)

orderForClosures
    :: Types.InferEnv -> Safe Var -> RuleBody SourceSpan
    -> (RuleBody SourceSpan, Unsafe Var SourceSpan)
orderForClosures inferEnv safe body =
    let (_, body', unsafes) = reorder step () body in
    (body', mconcat unsafes)
  where
    -- Variables appearing in the body.
    bodyVars = HS.toHashSetOf
        (ruleBodyTerms . termCosmosNoClosures . termNames . traverse . _LocalName)
        body

    -- Order predicate.
    step () reordered lit =
        -- Variables appearing in closures in this statement.
        let inClosureVars = HS.toHashSetOf
                (literalTerms . termCosmosClosures . comprehensionTerms .
                    termCosmosNames . traverse . _LocalName)
                lit

            -- Variabels that are both in the body as well as in the closures
            -- must be bound first.
            needOutVars =
                (bodyVars `HS.intersection` inClosureVars) `HS.difference`
                unSafe safe

            -- The current output variables.
            Safe currentOutVars = ovRuleBody inferEnv safe reordered

            -- Missing output variables.
            missing = needOutVars `HS.difference` currentOutVars

            -- Compute unsafe variables.  Should be empty in the best case.
            unsafes =
                const (NonEmpty.singleton (lit ^. literalAnn)) <$>
                HS.toMap missing in

        if HMS.null unsafes then OrderOk () else OrderError (Unsafe unsafes)

orderForSafety
    :: Types.InferEnv -> Safe Var -> RuleBody SourceSpan
    -> (RuleBody SourceSpan, Unsafe Var SourceSpan)
orderForSafety inferEnv safe0 body0
    -- If ordering for closures fails, shortcut here.
    | not (HMS.null (unUnsafe unsafes1)) = (body1, unsafes1)
    -- Otherwise, do a proper ordering.
    | otherwise =
        let idxBody = zip [0 :: Int ..] body1

            -- A list of unsafe variables per statement.
            unsafes = Map.fromList $ do
                (idx, lit) <- idxBody
                let inLit = HS.toHashSetOf
                        (literalTerms . termCosmosNoClosures . termNames . traverse . _LocalName)
                        lit
                return (idx, inLit `HS.difference` unSafe safe0)

            -- Order statements in this body.
            (_, body2, unsafes2) = reorder step (safe0, unsafes) idxBody

            -- Final run to recursively call `orderForSafety` on closures within
            -- terms in this body.
            ((_, body3), unsafes3) = runWriter $ mapAccumM
                (\safe lit -> do
                    lit' <- recurse safe lit
                    return (safe <> ovLiteral inferEnv safe lit', lit'))
                safe0
                (map snd body2) in

        (body3, unsafes1 <> mconcat unsafes2 <> unsafes3)
  where
    (body1, unsafes1) = orderForClosures inferEnv safe0 body0

    step
        :: (Safe Var, Map.Map Int (HS.HashSet Var))
        -> [(Int, Literal SourceSpan)]
        -> (Int, Literal SourceSpan)
        -> OrderPredicate
            (Safe Var, Map.Map Int (HS.HashSet Var))
            (Unsafe Var SourceSpan)

    step (safe, unsafes) _ordered (idx, lit)
        -- Find the unsafes we previously stored for this literal.
        | HMS.null stillUnsafe = OrderOk (nowSafe, Map.delete idx unsafes)
        | otherwise            = OrderError (Unsafe stillUnsafe)
      where
        nowSafe     = safe <> ovLiteral inferEnv safe lit
        prevUnsafes = fromMaybe mempty (Map.lookup idx unsafes)
        stillUnsafe =
            const (NonEmpty.singleton (lit ^. literalAnn)) <$>
            HS.toMap (prevUnsafes `HS.difference` unSafe nowSafe)

    -- Recursively rewrite all closures in a literal using the given safe list.
    recurse
        :: Safe Var -> Literal SourceSpan
        -> Writer (Unsafe Var SourceSpan) (Literal SourceSpan)
    recurse rsafe =
        traverseOf literalTerms (writer . orderTermForSafety inferEnv rsafe)

orderTermForSafety
    :: Types.InferEnv -> Safe Var -> Term SourceSpan
    -> (Term SourceSpan, Unsafe Var SourceSpan)
orderTermForSafety inferEnv safe =
    runWriter .
    traverseOf termRuleBodies (writer . orderForSafety inferEnv safe)

inferOutVars
    :: Types.InferEnv -> Safe Var -> Maybe SourceSpan -> Types.InferM a
    -> Safe Var
inferOutVars inferEnv safe mbSource infer =
    case maybeInferred of
        Nothing       -> Safe $ HS.empty
        Just (_, env) -> Safe $ Unification.keys env
  where
    maybeInferred =
        snd $ runIdentity $ runParachuteT $
        Types.runInfer inferEnv {Types._ieInferClosures = False} $ do
            for_ mbSource $ \source -> Types.setInferContext source $
                HS.toMap (unSafe safe) $> Types.unknown
            infer

ovRuleBody :: Types.InferEnv -> Safe Var -> RuleBody SourceSpan -> Safe Var
ovRuleBody inferEnv safe body =
    inferOutVars inferEnv safe (view literalAnn <$> listToMaybe body) (Types.inferRuleBody body)

ovLiteral :: Types.InferEnv -> Safe Var -> Literal SourceSpan -> Safe Var
ovLiteral inferEnv safe lit =
    inferOutVars inferEnv safe
    (Just $ lit ^. literalAnn) (Types.inferLiteral lit)
