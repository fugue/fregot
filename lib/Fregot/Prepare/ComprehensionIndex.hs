{-# LANGUAGE LambdaCase #-}
module Fregot.Prepare.ComprehensionIndex
    ( rewriteRule
    , rewriteLiteral
    ) where

import           Control.Lens              (over, (&), (.~), (^.), (^?))
import           Control.Lens.Plated       -- (transformOn, transformOnOf, plate)
import qualified Data.HashSet              as HS
import           Data.Maybe                (fromMaybe)
import qualified Data.Unique               as Unique
import           Debug.Trace
import           Fregot.Compile.Order      (Safe (..), ovLiteral, ovRuleBody)
import           Fregot.Names              (Name (..), UnqualifiedVar)
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)
import qualified Fregot.Types.Infer        as Types
import           System.IO.Unsafe          (unsafePerformIO)

rewriteRule :: Types.InferEnv -> Rule i SourceSpan -> Rule i SourceSpan
rewriteRule inferEnv =
    -- Top-level transform.
    over (ruleDefs . traverse . ruleDefinitionBodies) (rewriteBody inferEnv) .
    -- Transform nested closures.
    transformOn ruleTerms (over termRuleBodies (rewriteBody inferEnv))

indexUniqueGen :: Unique.GlobalUniqueGen
indexUniqueGen = unsafePerformIO Unique.newGlobalUniqueGen
{-# NOINLINE indexUniqueGen #-}

rewriteBody :: Types.InferEnv -> RuleBody SourceSpan -> RuleBody SourceSpan
rewriteBody inferEnv = go mempty
  where
    go _ [] = []
    go safe (lit : lits) =
        let lit' = fromMaybe lit $ rewriteLiteral inferEnv safe lit
            safe' = safe <> fromMaybe mempty (ovLiteral inferEnv safe lit') in
        lit' : go safe' lits

rewriteLiteral
    :: Types.InferEnv -> Safe Var -> Literal SourceSpan
    -> Maybe (Literal SourceSpan)
rewriteLiteral inferEnv safe lit
    -- Ignore negated statements.
    | lit ^. literalNegation = Nothing
    -- Ignore statements that have `with`s.
    | not $ null (lit ^. literalWith) = Nothing
    -- There needs to be an assignment.
    | Just (source, name, term) <- asAssignment (lit ^. literalStatement)
            -- With a comprehension as the RHS.
            , Just (_, comp) <- term ^? termToClosure
            -- Try computing the output variables of the body.  If it works,
            -- we can safely run the body on the top level.
            , Just ovs <- ovRuleBody inferEnv mempty (comp ^. comprehensionBody)
            , keys <- unSafe safe `HS.intersection` unSafe ovs
            , not (HS.null keys) =
        trace ("Lifted body: " ++ show (PP.pretty' comp)) $
        trace ("Keys: " ++ show keys) $
        Just $
        Unique.getGlobalUnique indexUniqueGen $ \unique ->
        lit & literalStatement .~ IndexedCompS source
            (IndexedComprehension unique (HS.toList keys) name comp)
    | otherwise = Nothing
  where
    asAssignment :: Statement a -> Maybe (a, UnqualifiedVar, Term a)
    asAssignment = \case
        AssignS source (NameT _ (LocalName v)) t -> Just (source, v, t)
        UnifyS  source (NameT _ (LocalName v)) t -> Just (source, v, t)
        _                                        -> Nothing
