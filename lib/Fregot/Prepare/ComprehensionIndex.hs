module Fregot.Prepare.ComprehensionIndex
    ( rewriteRule
    , rewriteLiteral
    ) where

import           Control.Lens              (over, (&), (.~), (^.))
import           Control.Lens.Plated       -- (transformOn, transformOnOf, plate)
import qualified Data.HashSet              as HS
import           Data.Maybe                (fromMaybe)
import qualified Data.Unique               as Unique
import           Debug.Trace
import           Fregot.Compile.Order      (Safe (..), ovLiteral, ovRuleBody)
import           Fregot.Names              (Name (..))
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
        -- trace ("Rewrote lit: " ++ show (PP.pretty' lit)) $
        let lit' = fromMaybe lit $ rewriteLiteral inferEnv safe lit
            safe' = safe <> fromMaybe mempty (ovLiteral inferEnv safe lit') in
        lit' : go safe' lits

rewriteLiteral
    :: Types.InferEnv -> Safe Var -> Literal SourceSpan
    -> Maybe (Literal SourceSpan)
rewriteLiteral inferEnv safe lit
    | lit ^. literalNegation = Nothing
    | not $ null (lit ^. literalWith) = Nothing
    | otherwise = case lit ^. literalStatement of
        -- TODO(jaspervdj): Add UnifyS, and other comprehensions.
        AssignS source (NameT _ (LocalName name)) (CompT _ comp@(ArrayComp _ body))
                -- Try computing the output variables of the body.  If it works,
                -- we can safely run the body on the top level.
                | Just ovs <- ovRuleBody inferEnv mempty body
                , keys <- unSafe safe `HS.intersection` unSafe ovs
                , not (HS.null keys) ->
            trace ("Lifted body: " ++ show (PP.pretty' comp)) $
            trace ("Keys: " ++ show keys) $
            Just $
            Unique.getGlobalUnique indexUniqueGen $ \unique ->
            lit & literalStatement .~ IndexedCompS source
                (IndexedComprehension unique (HS.toList keys) name comp)
        _ -> Nothing
