-- | Horrible DSL to be able to quickly construct syntax for use in tests.
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fregot.Prepare.Dsl where

import           Data.String        (IsString (..))
import           Fregot.Names
import           Fregot.Prepare.Ast

instance IsString Name where
    fromString = LocalName . fromString

lit :: Statement Int -> Literal Int
lit = literal 0

name :: Name -> Term Int
name = NameT 0

num :: Int -> Term Int
num = ScalarT 0 . Number . fromIntegral

call :: Name -> [Term Int] -> Term Int
call v args = CallT 0 (NamedFunction v) args
