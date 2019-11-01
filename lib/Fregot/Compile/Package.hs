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

import           Control.Applicative          ((<|>))
import           Control.Lens                 (at, iforM_, ix, traverseOf, view,
                                               (&), (.~), (^.), (^?))
import           Control.Monad                (foldM, forM, guard)
import           Control.Monad.Identity       (runIdentity)
import           Control.Monad.Parachute      (ParachuteT, catch, fatal,
                                               tellError, tellErrors)
import qualified Data.Graph                   as Graph
import qualified Data.HashMap.Strict.Extended as HMS
import qualified Data.HashSet.Extended        as HS
import           Data.List.NonEmpty.Extended  (NonEmpty (..))
import           Data.Proxy                   (Proxy (..))
import           Data.Traversable             (for)
import           Data.Traversable.HigherOrder (htraverse)
import           Fregot.Compile.Graph
import           Fregot.Compile.Order
import           Fregot.Error                 (Error)
import qualified Fregot.Error                 as Error
import           Fregot.Eval.Builtins         (Builtins)
import qualified Fregot.Eval.Builtins         as Builtins
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens
import           Fregot.Prepare.Package
import           Fregot.Prepare.Vars          (Arities, Safe (..))
import           Fregot.PrettyPrint           ((<$$>), (<+>))
import qualified Fregot.PrettyPrint           as PP
import           Fregot.Sources.SourceSpan    (SourceSpan)
import qualified Fregot.Types.Infer           as Infer
import           Fregot.Types.Rule            (RuleType)
import           Fregot.Types.Value           (TypeContext)
import           Prelude                      hiding (head, lookup)

type CompiledPackage = Package RuleType

type Dependencies = HMS.HashMap PackageName CompiledPackage

aritiesFromPackage :: Builtins f -> Package ty -> Arities
aritiesFromPackage builtins prep = \func ->
    (do
        builtin <- HMS.lookup func builtins
        return $ Builtins.arity builtin) <|>
    (do
        NamedFunction (QualifiedName pkg fname) <- Just func
        guard $ pkg == prep ^. packageName
        userdef <- lookup fname prep
        FunctionRule arity <- Just (userdef ^. ruleKind)
        return arity)

aritiesFromDependencies :: Dependencies -> Arities
aritiesFromDependencies deps = \func -> do
    NamedFunction (QualifiedName pkgname fname) <- Just func
    pkg                                         <- HMS.lookup pkgname deps
    userdef                                     <- lookup fname pkg
    FunctionRule arity                          <- Just (userdef ^. ruleKind)
    return arity

-- | Construct the safe-to-use global variables.  We don't need to look at the
-- rules in the package since the names referring to that have been turned into
-- 'QualifiedName' rather than 'LocalName' already.
safeGlobals :: Package ty -> Safe Var
safeGlobals _prep = Safe ["data", "input"]

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

    -- Simple compilation and checks.
    compiled0 <- for ordering compileRule

    -- This is a version of dependencies with a placeholder for the current
    -- package, so we can access info about the current package the same as we
    -- could other package.
    let vdependencies = HMS.insert pkgname
            (prep & packageRules .~ HMS.empty) dependencies

    let inferEnv0 = Infer.InferEnv
            -- TODO(jaspervdj): Builtins Proxy works quite well, we should move
            -- it up in the call stack.
            { Infer._ieBuiltins = runIdentity $
                traverse (htraverse $ \_ -> pure Proxy) builtins
            , Infer._ieDependencies = vdependencies
            }

    inferEnv1 <- foldM (\inferEnv rule ->
        (do
            tyRule <- Infer.runInfer inferEnv $ Infer.inferRule rule
            return $ inferEnv & Infer.ieDependencies . ix pkgname .
                packageRules .  at (rule ^. ruleName) .~ Just tyRule) `catch`
        -- TODO(jaspervdj): We'll want to replace them with error nodes and an
        -- 'unknown' type.
        (\errs -> do
            tellErrors errs
            return inferEnv))
        inferEnv0
        compiled0

    case inferEnv1 ^? Infer.ieDependencies . ix pkgname of
        Just p  -> return p
        Nothing -> fatal $ Error.mkErrorNoMeta
            "compile" "Package that we are compiling went missing"
  where
    pkgname     = prep ^. packageName
    selfArities = aritiesFromPackage builtins prep

    compileRule
        :: Monad m => Rule' -> ParachuteT Error m Rule'
    compileRule = traverseOf (ruleDefs . traverse) compileRuleDefinition

    compileRuleDefinition
        :: forall m. Monad m
        => RuleDefinition SourceSpan
        -> ParachuteT Error m (RuleDefinition SourceSpan)
    compileRuleDefinition def = do
        -- Order the bodies and terms.
        ordered <-
            traverseOf (ruleBodies . traverse) orderRuleBody def >>=
            traverseOf (ruleValue . traverse) orderTerm >>=
            traverseOf (ruleElses . traverse . ruleElseBody) orderRuleBody >>=
            traverseOf (ruleElses . traverse . ruleElseValue . traverse) orderTerm

        return ordered
      where
        -- Safe set before ordering.
        safe = safeGlobals prep <> safeLocals

        safeLocals = Safe $ HS.toHashSetOf
            (ruleArgs . traverse . traverse .
                termCosmosNoClosures . termNames . traverse . _LocalName)
            def

        orderRuleBody = runOrder . orderForSafety arities safe
        orderTerm = runOrder . orderTermForSafety arities safe

        arities = \f ->
            selfArities f <|>
            aritiesFromDependencies dependencies f

compileQuery
    :: Monad m
    => Builtins f
    -> Dependencies
    -> Package ty
    -> TypeContext
    -> Query SourceSpan
    -> ParachuteT Error m (Query SourceSpan)
compileQuery builtins dependencies pkg typeContext query0 = do
    query1 <- runOrder $ orderForSafety arities safe0 query0
    let inferEnv = Infer.InferEnv
            -- TODO(jaspervdj): Builtins Proxy works quite well, we should move
            -- it up in the call stack.
            { Infer._ieBuiltins = runIdentity $
                traverse (htraverse $ \_ -> pure Proxy) builtins
            , Infer._ieDependencies = dependencies
            }

    Infer.runInfer inferEnv $ do
        case query0 of
            []    -> pure ()
            l : _ -> Infer.setInferContext (l ^. literalAnn) typeContext
        Infer.inferQuery query1
    return query1
  where
    safe0   = safeGlobals pkg <> Safe (HMS.keysSet typeContext)
    arities = aritiesFromPackage builtins pkg

compileTerm
    :: Monad m
    => Builtins f
    -> Dependencies
    -> Package ty
    -> TypeContext
    -> Term SourceSpan
    -> ParachuteT Error m (Term SourceSpan, Infer.SourceType)
compileTerm builtins dependencies pkg typeContext term0 = do
    ordered <- runOrder $ orderTermForSafety arities safe0 term0

    let inferEnv = Infer.InferEnv
            -- TODO(jaspervdj): Builtins Proxy works quite well, we should move
            -- it up in the call stack.
            { Infer._ieBuiltins = runIdentity $
                traverse (htraverse $ \_ -> pure Proxy) builtins
            , Infer._ieDependencies = dependencies
            }

    ty <- Infer.runInfer inferEnv $ do
        Infer.setInferContext (term0 ^. termAnn) typeContext
        Infer.inferTerm ordered
    return (ordered, ty)
  where
    safe0   = safeGlobals pkg <> Safe (HMS.keysSet typeContext)
    arities = aritiesFromPackage builtins pkg

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
