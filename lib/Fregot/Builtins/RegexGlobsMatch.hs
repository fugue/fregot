{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Builtins.RegexGlobsMatch
    ( builtins
    ) where

import qualified Data.GlobIntersection    as GlobIntersection
import qualified Data.HashMap.Strict      as HMS
import qualified Data.Text                as T
import           Fregot.Builtins.Internal
import           Fregot.Names
import           Fregot.Types.Builtins    ((🡒))
import qualified Fregot.Types.Builtins    as Ty
import qualified Fregot.Types.Internal    as Ty

builtins :: Monad m => Builtins m
builtins = HMS.fromList
    [ (NamedFunction (QualifiedName "regex.globs_match"), builtin_regex_globs_match)
    ]

builtin_regex_globs_match :: Monad m => Builtin m
builtin_regex_globs_match = Builtin
    (Ty.string 🡒 Ty.string 🡒 Ty.out Ty.boolean) $ pure $ do
    \(Cons l (Cons r Nil)) -> eitherToBuiltinM $ do
        lp <- GlobIntersection.parse $ T.unpack l
        rp <- GlobIntersection.parse $ T.unpack r
        pure $ GlobIntersection.intersects lp rp
