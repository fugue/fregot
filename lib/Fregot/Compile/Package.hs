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

import           Control.Lens                (iforM, traverseOf, (^.))
import           Control.Monad.Parachute     (ParachuteT, tellError)
import qualified Data.HashMap.Strict         as HMS
import qualified Data.HashSet.Extended       as HS
import           Data.List.NonEmpty.Extended (NonEmpty (..))
import           Fregot.Compile.Order
import           Fregot.Error                (Error)
import qualified Fregot.Error                as Error
import qualified Fregot.Eval.Builtins        as Builtins
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens
import           Fregot.Prepare.Package
import           Fregot.Prepare.Vars         (Arities, Safe (..))
import           Fregot.PrettyPrint          ((<+>))
import qualified Fregot.PrettyPrint          as PP
import           Fregot.Sources.SourceSpan   (SourceSpan)
import           Prelude                     hiding (head, lookup)

type CompiledPackage = Package ()

arities :: Arities
arities = \func -> case HMS.lookup func Builtins.builtins of
    -- TODO(jaspervdj): User-defined function arities.
    Nothing      -> 0
    Just builtin -> Builtins.arity builtin

-- | Construct the safe-to-use global variables.
safeGlobals :: PreparedPackage -> Safe Var
safeGlobals prep = Safe $ HS.fromList (rules prep) <> ["data", "input"]

compilePackage
    :: Monad m => PreparedPackage -> ParachuteT Error m CompiledPackage
compilePackage prep =
    traverseOf (packageRules . traverse) compileRule prep
  where
    compileRule
        :: Monad m => Rule SourceSpan -> ParachuteT Error m (Rule SourceSpan)
    compileRule = traverseOf (ruleDefs . traverse) compileRuleDefinition

    compileRuleDefinition
        :: forall m. Monad m
        => RuleDefinition SourceSpan
        -> ParachuteT Error m (RuleDefinition SourceSpan)
    compileRuleDefinition def =
        traverseOf (ruleBodies . traverse) orderRuleBody def >>=
        traverseOf (ruleElses . traverse . ruleElseBody) orderRuleBody >>=
        traverseOf (ruleValue . traverse ) orderTerm
      where
        safe = safeGlobals prep <> safeLocals <> safeImports

        safeLocals = Safe $ HS.toHashSetOf
            (ruleArgs . traverse . traverse . termCosmosNoClosures . termVars)
            def

        safeImports = Safe $ HS.fromMap $ () <$ def ^. ruleDefImports

        orderRuleBody = runOrder . orderForSafety arities safe
        orderTerm = runOrder . orderTermForSafety arities safe

compileTerm
    :: Monad m
    => PreparedPackage -> Term SourceSpan
    -> ParachuteT Error m (Term SourceSpan)
compileTerm pkg = runOrder . orderTermForSafety arities (safeGlobals pkg)

-- | Designed to match the return type of `orderTermForSafety`.
runOrder
    :: (Monad m, PP.Pretty PP.Sem v)
    => (a, Unsafe v SourceSpan) -> ParachuteT Error m a
runOrder (x, Unsafe unsafe) = do
    _ <- iforM unsafe $ \var (source :| _) -> tellError $ Error.mkError
        "compile" source "unknown variable" $
        "Undefined variable:" <+> PP.pretty var
    return x
