{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Fregot.Compile.Package
    ( CompiledPackage
    , lookup
    , rules
    , compilePackage
    , compileTerm
    ) where

import           Control.Applicative         ((<|>))
import           Control.Lens                (forOf_, iforM_, traverseOf, (^.),
                                              (^..))
import           Control.Monad.Parachute     (ParachuteT, tellError, tellErrors)
import qualified Data.HashMap.Strict         as HMS
import qualified Data.HashSet.Extended       as HS
import           Data.List.NonEmpty.Extended (NonEmpty (..))
import           Data.Maybe                  (isNothing)
import           Fregot.Compile.Order
import           Fregot.Error                (Error)
import qualified Fregot.Error                as Error
import qualified Fregot.Eval.Builtins        as Builtins
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens
import           Fregot.Prepare.Package
import           Fregot.Prepare.Vars         (Arities, Safe (..), ovRuleBody)
import           Fregot.PrettyPrint          ((<$$>), (<+>))
import qualified Fregot.PrettyPrint          as PP
import           Fregot.Sources.SourceSpan   (SourceSpan)
import           Prelude                     hiding (head, lookup)

type CompiledPackage = Package ()

aritiesFromPackage :: PreparedPackage -> Arities
aritiesFromPackage prep = \func ->
    (do
        builtin <- HMS.lookup func Builtins.builtins
        return $ Builtins.arity builtin) <|>
    (do
        -- TODO(jaspervdj): Look up functions in other packages as well.
        NamedFunction [fname] <- Just func
        userdef <- lookup fname prep
        FunctionRule arity <- Just (userdef ^. ruleKind)
        return arity)

-- | Construct the safe-to-use global variables.
safeGlobals :: PreparedPackage -> Safe Var
safeGlobals prep = Safe $ HS.fromList (rules prep) <> ["data", "input"]

compilePackage
    :: Monad m => PreparedPackage -> ParachuteT Error m CompiledPackage
compilePackage prep =
    traverseOf (packageRules . traverse) compileRule prep
  where
    arities = aritiesFromPackage prep

    compileRule
        :: Monad m => Rule SourceSpan -> ParachuteT Error m (Rule SourceSpan)
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

        -- General check the elses (index and else value).
        forOf_ (ruleElses . traverse) ordered $ \els -> do
            let safe1 = safe <> ovRuleBody arities safe (els ^. ruleElseBody)
            forOf_ (ruleIndex . traverse) def $ \v ->
                tellErrors $ checkTerm arities safe1 v
            forOf_ (ruleElseValue . traverse) els $ \v ->
                tellErrors $ checkTerm arities safe1 v

        -- Call check inside rule bodies.
        forOf_ (ruleBodies . traverse . ruleBodyTerms) ordered $
            \term -> tellErrors $ checkTermCalls arities term

        return ordered
      where
        -- Safe set before ordering.
        safe = safeGlobals prep <> safeLocals <> safeImports

        safeLocals = Safe $ HS.toHashSetOf
            (ruleArgs . traverse . traverse .
                termCosmosNoClosures . termVars . traverse)
            def

        safeImports = Safe $ HS.fromMap $ () <$ def ^. ruleDefImports

        orderRuleBody = runOrder . orderForSafety arities safe
        orderTerm = runOrder . orderTermForSafety arities safe

compileTerm
    :: Monad m
    => PreparedPackage -> Term SourceSpan
    -> ParachuteT Error m (Term SourceSpan)
compileTerm pkg term0 = do
    ordered <- runOrder $ orderTermForSafety arities safe term0
    tellErrors $ checkTerm arities safe ordered
    return ordered
  where
    safe    = safeGlobals pkg
    arities = aritiesFromPackage pkg

-- | Various checks on terms.
checkTerm :: Arities -> Safe Var -> Term SourceSpan -> [Error]
checkTerm arities safe term =
    checkTermVars safe term <>
    checkTermCalls arities term

-- | Check that all variables in the term occurr in the safe set.
checkTermVars
    :: Safe Var
    -> Term SourceSpan
    -> [Error]
checkTermVars (Safe safe) term =
    [ Error.mkError "var check" source "unsafe variable" $
        "Variable" <+> PP.pretty var <+> "is referenced, but it is never" <$$>
        "assigned a value."
    | (source, var) <- term ^.. termCosmosNoClosures . termVars
    , not $ var `HS.member` safe
    ]

-- | Check that all calls in the term are known functions.
checkTermCalls
    :: Arities
    -> Term SourceSpan
    -> [Error]
checkTermCalls arities term =
    [ Error.mkError "var check" source "unknown function" $
        "Function" <+> PP.pretty fun <+> "is not defined."
    | (source, fun) <- term ^.. termCosmosCalls
    , isNothing (arities fun)
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
