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
    , compileTerm
    ) where

import           Control.Applicative          ((<|>))
import           Control.Lens                 (at, forOf_, iforM_, ix,
                                               traverseOf, view, (&), (.~),
                                               (^.), (^..), (^?))
import           Control.Monad                (foldM, forM, guard, when)
import           Control.Monad.Identity       (runIdentity)
import           Control.Monad.Parachute      (ParachuteT, fatal, tellError,
                                               tellErrors)
import           Data.Functor                 (($>))
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
import           Fregot.Prepare.Vars          (Arities, Safe (..), ovRuleBody,
                                               ovTerm)
import           Fregot.PrettyPrint           ((<$$>), (<+>))
import qualified Fregot.PrettyPrint           as PP
import           Fregot.Sources.SourceSpan    (SourceSpan)
import qualified Fregot.TypeCheck.Infer       as Infer
import           Fregot.TypeCheck.Types
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
        Graph.CyclicSCC  cycl -> tellError (recursionError cycl) $> cycl

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

    inferEnv1 <- foldM (\inferEnv rule -> do
        tyRule <- Infer.runInfer inferEnv $ Infer.inferRule rule
        return $ inferEnv & Infer.ieDependencies . ix pkgname . packageRules .
            at (rule ^. ruleName) .~ Just tyRule)
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

        -- General check for every rule body (index and value).
        forOf_ (ruleBodies . traverse) ordered $ \body -> do
            let safe1 = safe <> ovRuleBody arities safe body
            forOf_ (ruleIndex . traverse) def $ \v ->
                tellErrors $ checkTerm arities safe1 v
            forOf_ (ruleValue . traverse) def $ \v ->
                tellErrors $ checkTerm arities safe1 v

        -- If there is no body; we still need the index and value to be defined.
        when (null (ordered ^. ruleBodies)) $
            forOf_ (ruleValue . traverse) def $ \v ->
            tellErrors $ checkTerm arities safe v

        -- General check the elses (index and else value).
        forOf_ (ruleElses . traverse) ordered $ \els -> do
            let safe1 = safe <> ovRuleBody arities safe (els ^. ruleElseBody)
            forOf_ (ruleIndex . traverse) def $ \v ->
                tellErrors $ checkTerm arities safe1 v
            forOf_ (ruleElseValue . traverse) els $ \v ->
                tellErrors $ checkTerm arities safe1 v

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

compileTerm
    :: Monad m
    => Builtins f -> Package ty -> Safe Var -> Term SourceSpan
    -> ParachuteT Error m (Term SourceSpan)
compileTerm builtins pkg safeLocals term0 = do
    ordered <- runOrder $ orderTermForSafety arities safe0 term0
    let safe = safe0 <> ovTerm arities safe0 ordered
    tellErrors $ checkTerm arities safe ordered
    return ordered
  where
    safe0   = safeGlobals pkg <> safeLocals
    arities = aritiesFromPackage builtins pkg

-- | Various checks on terms.
checkTerm :: Arities -> Safe Var -> Term SourceSpan -> [Error]
checkTerm _arities safe term = checkTermVars safe term

-- | Check that all variables in the term occurr in the safe set.
checkTermVars
    :: Safe Var
    -> Term SourceSpan
    -> [Error]
checkTermVars (Safe safe) term =
    [ Error.mkError "var check" source "unsafe variable" $
        "Variable" <+> PP.pretty var <+> "is referenced, but it is never" <$$>
        "assigned a value."
    | (source, name) <- term ^.. termCosmosNoClosures . termNames
    , var            <- name ^.. _LocalName
    , not $ var `HS.member` safe
    ]

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
