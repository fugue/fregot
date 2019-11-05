{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Fregot.Compile.Package
    ( Safe (..)
    , CompiledPackage, packageName, packageRules
    , lookup
    , rules
    , compilePackage
    , compileQuery
    , compileTerm
    ) where

import           Control.Lens                 (at, iforM_, ix, traverseOf, view,
                                               (&), (.~), (^.), (^?))
import           Control.Monad                (foldM, forM)
import           Control.Monad.Identity       (runIdentity)
import           Control.Monad.Parachute      (ParachuteT, catch, fatal,
                                               tellError, tellErrors)
import qualified Data.Graph                   as Graph
import qualified Data.HashMap.Strict.Extended as HMS
import qualified Data.HashSet.Extended        as HS
import           Data.List.NonEmpty.Extended  (NonEmpty (..))
import           Data.Proxy                   (Proxy (..))
import           Data.Traversable.HigherOrder (htraverse)
import           Fregot.Compile.Graph
import           Fregot.Compile.Order
import           Fregot.Error                 (Error)
import qualified Fregot.Error                 as Error
import           Fregot.Eval.Builtins         (Builtins)
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens
import           Fregot.Prepare.Package
import           Fregot.Prepare.Vars          (Safe (..))
import           Fregot.PrettyPrint           ((<$$>), (<+>))
import qualified Fregot.PrettyPrint           as PP
import           Fregot.Sources.SourceSpan    (SourceSpan)
import qualified Fregot.Types.Infer           as Infer
import           Fregot.Types.Rule            (RuleType)
import           Fregot.Types.Value           (TypeContext)
import           Prelude                      hiding (head, lookup)

type CompiledPackage = Package RuleType

type Dependencies = HMS.HashMap PackageName CompiledPackage

compilePackage
    :: Monad m
    => Builtins (f :: * -> *)
    -> Dependencies
    -> PreparedPackage
    -> ParachuteT Error m CompiledPackage
compilePackage builtins dependencies prep = do
    -- Build dependency graph.
    let graph = do
            rule <- fmap snd $ HMS.toList $ prep ^. packageRules
            let key = (rule ^. rulePackage, rule ^. ruleName)
            return (rule, key, HS.toList $ ruleDependencies rule)

    -- Order rules according to dependency graph.
    ordering <- fmap concat $ forM (Graph.stronglyConnComp graph) $ \case
        Graph.AcyclicSCC rule -> return [rule]
        Graph.CyclicSCC  cycl ->
            -- If rules have recursion errors, we drop them here.
            --
            -- TODO(jaspervdj): We'll want to replace them with error nodes.
            tellError (recursionError cycl) >> return []

    -- This is a version of dependencies with a placeholder for the current
    -- package, so we can access info about the current package the same as we
    -- could other package.
    let vdependencies = HMS.insert pkgname
            (prep & packageRules .~ HMS.empty) dependencies

    let inferEnv0 = Infer.emptyInferEnv
            -- TODO(jaspervdj): Builtins Proxy works quite well, we should move
            -- it up in the call stack.
            { Infer._ieBuiltins = runIdentity $
                traverse (htraverse $ \_ -> pure Proxy) builtins
            , Infer._ieDependencies = vdependencies
            }

    inferEnv1 <- foldM (\inferEnv rule ->
        (do
            -- Simple compilation and checks.
            cRule  <- compileRule inferEnv rule
            tyRule <- Infer.evalInfer inferEnv $ Infer.inferRule cRule
            return $ inferEnv & Infer.ieDependencies . ix pkgname .
                packageRules .  at (rule ^. ruleName) .~ Just tyRule) `catch`
        -- TODO(jaspervdj): We'll want to replace them with error nodes and an
        -- 'unknown' type.
        (\errs -> do
            tellErrors errs
            return inferEnv))
        inferEnv0
        ordering

    case inferEnv1 ^? Infer.ieDependencies . ix pkgname of
        Just p  -> return p
        Nothing -> fatal $ Error.mkErrorNoMeta
            "compile" "Package that we are compiling went missing"
  where
    pkgname = prep ^. packageName

    compileRule
        :: Monad m => Infer.InferEnv -> Rule' -> ParachuteT Error m Rule'
    compileRule ie = traverseOf (ruleDefs . traverse) (compileRuleDefinition ie)

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
        --
        -- TODO(jaspervdj): With InferEnv, safeGlobals should not be necessary
        -- anymore.
        safe = safeLocals

        safeLocals = Safe $ HS.toHashSetOf
            (ruleArgs . traverse . traverse .
                termCosmosNoClosures . termNames . traverse . _LocalName)
            def

        orderRuleBody = runOrder . orderForSafety ie safe
        orderTerm = runOrder . orderTermForSafety ie safe

compileQuery
    :: Monad m
    => Builtins f
    -> Dependencies
    -> TypeContext
    -> Query SourceSpan
    -> ParachuteT Error m (Query SourceSpan)
compileQuery builtins dependencies typeContext query0 = do
    let inferEnv = Infer.emptyInferEnv
            -- TODO(jaspervdj): Builtins Proxy works quite well, we should move
            -- it up in the call stack.
            { Infer._ieBuiltins = runIdentity $
                traverse (htraverse $ \_ -> pure Proxy) builtins
            , Infer._ieDependencies = dependencies
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
    -> Dependencies
    -> TypeContext
    -> Term SourceSpan
    -> ParachuteT Error m (Term SourceSpan, Infer.SourceType)
compileTerm builtins dependencies typeContext term0 = do
    let inferEnv = Infer.emptyInferEnv
            -- TODO(jaspervdj): Builtins Proxy works quite well, we should move
            -- it up in the call stack.
            { Infer._ieBuiltins = runIdentity $
                traverse (htraverse $ \_ -> pure Proxy) builtins
            , Infer._ieDependencies = dependencies
            }

    ordered <- runOrder $ orderTermForSafety inferEnv safe0 term0
    ty <- Infer.evalInfer inferEnv $ do
        Infer.setInferContext (term0 ^. termAnn) typeContext
        Infer.inferTerm ordered
    return (ordered, ty)
  where
    safe0 = Safe (HMS.keysSet typeContext)

-- | Designed to match the return type of `orderTermForSafety`.
runOrder
    :: (Monad m, PP.Pretty PP.Sem v)
    => (a, Unsafe v SourceSpan) -> ParachuteT Error m a
runOrder (x, Unsafe unsafe) = do
    iforM_ unsafe $ \var (source :| _) -> tellError $ Error.mkError
        "compile" source "unknown variable" $
        "Undefined variable:" <+> PP.pretty var
    return x

recursionError :: [Rule ty a] -> Error
recursionError [single] = Error.mkError
    "recursion check" (single ^. ruleAnn) "rule is recursive"
    "This rule is recursive."
recursionError cycl = Error.mkMultiError
    "recursion check"
    "rules are recursive" $ do
        r <- cycl
        return $ (,) (r ^. ruleAnn) $
            "These rules are mutually recursive:" <$$>
            PP.ind (PP.vcat (map (PP.code . PP.pretty . view ruleName) cycl))
