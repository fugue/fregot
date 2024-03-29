{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
module Fregot.Compile.Package
    ( Safe (..)
    , CompiledRule
    , compileTree
    , compileQuery
    , compileTerm

    , valueToCompiledRule
    ) where

import           Control.Lens                      (at, review, traverseOf,
                                                    view, (&), (.~), (^.))
import           Control.Monad                     (foldM, forM)
import           Control.Monad.Parachute           (ParachuteT, catching,
                                                    tellError, tellErrors)
import           Data.Bifunctor                    (first)
import           Data.Foldable                     (for_)
import           Data.Functor                      (($>))
import qualified Data.Graph                        as Graph
import qualified Data.HashMap.Strict.Extended      as HMS
import qualified Data.HashSet.Extended             as HS
import           Data.Kind                         (Type)
import           Data.List                         (sortOn)
import           Data.List.NonEmpty.Extended       (NonEmpty (..))
import           Fregot.Builtins.Internal          (Builtins)
import           Fregot.Compile.Graph
import           Fregot.Compile.Internal
import           Fregot.Compile.Order
import           Fregot.Dump                       (MonadDump, dump)
import           Fregot.Error                      (Error)
import qualified Fregot.Error                      as Error
import           Fregot.Eval.Value                 (Value)
import           Fregot.Names
import           Fregot.Prepare.Ast
import qualified Fregot.Prepare.BottomUp           as BottomUp
import qualified Fregot.Prepare.ComprehensionIndex as ComprehensionIndex
import qualified Fregot.Prepare.ConstantFold       as ConstantFold
import           Fregot.Prepare.Lens
import           Fregot.Prepare.Package
import           Fregot.PrettyPrint                ((<$$>), (<+>))
import qualified Fregot.PrettyPrint                as PP
import           Fregot.Sources.SourceSpan         (SourceSpan)
import qualified Fregot.Tree                       as Tree
import qualified Fregot.Types.Infer                as Infer
import           Fregot.Types.Rule                 (RuleType (..))
import           Fregot.Types.Value                (TypeContext, inferValue)
import           Prelude                           hiding (head, lookup)

-- | Compiles and merges the prepared tree into the compiled tree.
compileTree
    :: MonadDump m
    => Builtins (f :: Type -> Type)
    -> Tree.Tree CompiledRule
    -> Tree.Tree PreparedRule
    -> ParachuteT Error m (Tree.Tree CompiledRule)
compileTree builtins ctree0 prep = do
    -- Build dependency graph.
    let graph = do
            (key, rule) <- Tree.toList prep
            return (rule, key, HS.toList $ ruleDependencies prep rule)

    -- Order rules according to dependency graph.
    ordering <- fmap concat $ forM (Graph.stronglyConnComp graph) $ \case
        Graph.AcyclicSCC rule -> return [rule]
        Graph.CyclicSCC  cycl -> do
            tellError (recursionError cycl)
            pure $ fmap (ruleKind .~ ErrorRule) cycl

    ctree1 <- foldM
        (\ctree rule -> do
            let key = review Tree.qualifiedVarFromKey
                            (rule ^. rulePackage, rule ^. ruleName)
                inferEnv = Infer.InferEnv
                    { Infer.ieBuiltins      = builtins
                    , Infer.ieTree          = ctree
                    , Infer.ieInferClosures = Infer.ieInferClosures Infer.emptyInferEnv
                    }
            cRule  <- compileRule inferEnv rule
            tyRule <- catching
                (\errs -> if null errs then Nothing else Just errs)
                (Infer.evalInfer inferEnv $ Infer.inferRule cRule)
                (\errs -> tellErrors errs $> (rule & ruleInfo .~ ErrorType))
            let !optRule =
                    first (uncurry CompiledRuleInfo) $
                    BottomUp.addBottomUpInfo $
                    ComprehensionIndex.rewriteRule inferEnv $
                    ConstantFold.rewriteRule tyRule
            dump "opt" optRule
            pure $ ctree & at key .~ Just optRule)
        ctree0
        ordering

    pure ctree1

compileRule
    :: Monad m => Infer.InferEnv -> Rule' -> ParachuteT Error m Rule'
compileRule ie rule = catching
    (\errs -> if null errs then Nothing else Just errs)
    (traverseOf (ruleDefs . traverse) (compileRuleDefinition ie) rule)
    (\errs -> tellErrors errs $> (rule & ruleKind .~ ErrorRule))

compileRuleDefinition
    :: forall m. Monad m
    => Infer.InferEnv
    -> RuleDefinition SourceSpan
    -> ParachuteT Error m (RuleDefinition SourceSpan)
compileRuleDefinition ie def = do
    -- Order the bodies and terms.
    ordered <-
        traverseOf (ruleBodies . traverse) orderRuleBody def >>=
        traverseOf (ruleValue . traverse) orderTerm >>=
        traverseOf (ruleElses . traverse . ruleElseBody) orderRuleBody >>=
        traverseOf (ruleElses . traverse . ruleElseValue . traverse) orderTerm

    return ordered
  where
    -- Safe set before ordering.
    safe = Safe $ HS.toHashSetOf
        (ruleArgs . traverse . traverse .
            termCosmosNoClosures . termNames . traverse . _LocalName)
        def

    orderRuleBody = runOrder . orderForSafety ie safe
    orderTerm = runOrder . orderTermForSafety ie safe

compileQuery
    :: Monad m
    => Builtins f
    -> Tree.Tree CompiledRule
    -> TypeContext
    -> Query SourceSpan
    -> ParachuteT Error m (Query SourceSpan)
compileQuery builtins ctree typeContext query0 = do
    let inferEnv = Infer.InferEnv
            { Infer.ieBuiltins      = builtins
            , Infer.ieTree          = ctree
            , Infer.ieInferClosures = Infer.ieInferClosures Infer.emptyInferEnv
            }

    query1 <- runOrder $ orderForSafety inferEnv safe0 query0
    Infer.evalInfer inferEnv $ do
        case query0 of
            []    -> pure ()
            l : _ -> Infer.setInferContext (l ^. literalAnn) typeContext
        Infer.inferQuery query1
    return query1
  where
    safe0 = Safe (HMS.keysSet typeContext)

compileTerm
    :: Monad m
    => Builtins f
    -> Tree.Tree CompiledRule
    -> TypeContext
    -> Term SourceSpan
    -> ParachuteT Error m (Term SourceSpan, Infer.SourceType)
compileTerm builtins ctree typeContext term0 = do
    let inferEnv = Infer.InferEnv
            { Infer.ieBuiltins = builtins
            , Infer.ieTree = ctree
            , Infer.ieInferClosures = Infer.ieInferClosures Infer.emptyInferEnv
            }

    ordered <- runOrder $ orderTermForSafety inferEnv safe0 term0
    ty <- Infer.evalInfer inferEnv $ do
        Infer.setInferContext (term0 ^. termAnn) typeContext
        Infer.inferTerm ordered
    return (ordered, ty)
  where
    safe0 = Safe (HMS.keysSet typeContext)

-- | Designed to match the return type of `orderTermForSafety`.
runOrder :: Monad m => (a, Unsafe Var SourceSpan) -> ParachuteT Error m a
runOrder (x, Unsafe unsafe) = do
    for_ (sortOn (\(v, src :| _) -> (src, unVar v)) $ HMS.toList unsafe) $
        \(v, src :| _) -> tellError $ Error.mkError
        "compile" src "unknown variable" $
        "Undefined variable:" <+> PP.pretty v
    return x

recursionError :: [Rule ty a] -> Error
recursionError [single] = Error.mkError
    "recursion check" (single ^. ruleAnn) "rule is recursive"
    "This rule is recursive."
recursionError cycl = Error.mkMultiError
    "recursion check"
    "rules are recursive" $ do
        r <- cycl
        return $ (,) (r ^. ruleAnn) $ Just $
            "These rules are mutually recursive:" <$$>
            PP.ind (PP.vcat (map (PP.code . PP.pretty . view ruleName) cycl))

valueToCompiledRule :: SourceSpan -> PackageName -> Var -> Value -> CompiledRule
valueToCompiledRule source pkgname var val =
    termToRule source pkgname var (ValueT source val) &
    ruleInfo .~
        CompiledRuleInfo (CompleteRuleType (inferValue val)) BottomUp.TopDown
