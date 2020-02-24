-- | Horrible DSL to be able to quickly construct syntax for use in tests.
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fregot.Prepare.Dsl where

import           Control.Lens              (review, (&), (.~))
import qualified Data.HashMap.Strict       as HMS
import           Data.String               (IsString (..))
import qualified Fregot.Eval.Builtins      as B
import           Fregot.Eval.Number        (Number)
import           Fregot.Lexer.Position     (initPosition)
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens       (valueToScalar)
import           Fregot.Sources.SourceSpan (SourceSpan)
import qualified Fregot.Sources.SourceSpan as SourceSpan
import           Fregot.Types.Builtins     ((🡒))
import qualified Fregot.Types.Builtins     as Ty
import qualified Fregot.Types.Infer        as Types
import qualified Fregot.Types.Internal     as Ty

instance IsString Name where
    fromString = LocalName . fromString

lit :: Statement SourceSpan -> Literal SourceSpan
lit = literal source

name :: Name -> Term SourceSpan
name = NameT source

num :: Int -> Term SourceSpan
num = ValueT source . review valueToScalar . Number . fromIntegral

call :: Name -> [Term SourceSpan] -> Term SourceSpan
call v args = CallT source (NamedFunction v) args

source :: SourceSpan
source = SourceSpan.SourceSpan SourceSpan.TestInput initPosition initPosition

inferEnv :: Types.InferEnv
inferEnv = Types.emptyInferEnv & Types.ieBuiltins .~
    HMS.singleton (NamedFunction (BuiltinName "add")) builtin_add
  where
    builtin_add :: Monad m => B.Builtin m
    builtin_add = B.Builtin
        (B.In (B.In B.Out))
        (Ty.number 🡒 Ty.number 🡒 Ty.out Ty.number) $ return $
        \(B.Cons x (B.Cons y B.Nil)) ->
        return ((x :: Number) + (y :: Number) :: Number)
