{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Fregot.Compile.Package
    ( CompiledPackage
    , lookup
    , rules
    , compile
    ) where

import           Control.Lens                (iforM, traverseOf)
import           Control.Monad.Parachute     (ParachuteT, tellError)
import qualified Data.HashMap.Strict         as HMS
import qualified Data.HashSet                as HS
import           Data.List.NonEmpty.Extended (NonEmpty (..))
import           Fregot.Compile.Order
import           Fregot.Error                (Error)
import qualified Fregot.Error                as Error
import qualified Fregot.Eval.Builtins        as Builtins
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Package
import           Fregot.Prepare.Vars         (Arities, Safe (..))
import           Fregot.PrettyPrint          ((<+>))
import qualified Fregot.PrettyPrint          as PP
import           Fregot.Sources.SourceSpan   (SourceSpan)
import           Prelude                     hiding (head, lookup)

type CompiledPackage = Package ()

compile :: Monad m => PreparedPackage -> ParachuteT Error m CompiledPackage
compile prep =
    traverseOf (packageRules . traverse) compileRule prep
  where
    arities :: Arities
    arities = \func -> case HMS.lookup func Builtins.builtins of
        Nothing      -> 0
        Just builtin -> Builtins.arity builtin

    globals :: Safe Var
    globals = Safe $
        HS.fromList (rules prep) <>
        ["input"]

    compileRule
        :: Monad m => Rule SourceSpan -> ParachuteT Error m (Rule SourceSpan)
    compileRule = traverseOf (ruleDefs . traverse) compileRuleDefinition

    compileRuleDefinition
        :: forall m. Monad m
        => RuleDefinition SourceSpan
        -> ParachuteT Error m (RuleDefinition SourceSpan)
    compileRuleDefinition def =
        traverseOf (ruleBodies . traverse) order def >>=
        traverseOf (ruleElses . traverse . ruleElseBody) order
      where
        order :: RuleBody SourceSpan -> ParachuteT Error m (RuleBody SourceSpan)
        order b = do
            let (b', Unsafe unsafe) = orderForSafety arities globals b
            _ <- iforM unsafe $ \var (source :| _) -> tellError $ Error.mkError
                "compile" source "unknown variable" $
                "Undefined variable:" <+> PP.pretty var
            return b'
