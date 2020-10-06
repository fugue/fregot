{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

Regex-related builtins.
-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Builtins.Regex
    ( builtins
    ) where

import           Control.Monad.Trans      (liftIO)
import           Data.Bifunctor           (first)
import qualified Data.HashMap.Strict      as HMS
import           Data.IORef               (atomicModifyIORef', newIORef)
import qualified Data.Text                as T
import           Fregot.Builtins.Internal
import           Fregot.Names
import           Fregot.Types.Builtins    ((🡒))
import qualified Fregot.Types.Builtins    as Ty
import qualified Fregot.Types.Internal    as Ty
import qualified Text.Pcre2               as Pcre2

builtins :: Builtins IO
builtins = HMS.fromList
    [ (NamedFunction (QualifiedName "regex.split"), builtin_regex_split)
    , (NamedFunction (BuiltinName "re_match"),      builtin_re_match)
    ]

builtin_regex_split :: Builtin IO
builtin_regex_split = Builtin
    (Ty.string 🡒 Ty.string 🡒 Ty.out (Ty.arrayOf Ty.string)) $ do
    cacheRef <- newIORef HMS.empty
    pure $
        -- TODO(jaspervdj): Clean up duplication between this and
        -- `builtin_re_match`.
        \(Cons pattern (Cons value Nil)) -> do
        errOrRegex <- liftIO $ atomicModifyIORef' cacheRef $ \cache ->
            case HMS.lookup pattern cache of
                Just errOrRegex -> return errOrRegex
                Nothing         ->
                    let errOrRegex = Pcre2.compile pattern in
                    (HMS.insert pattern errOrRegex cache, errOrRegex)

        eitherToBuiltinM $ do
            regex <- first show errOrRegex
            match <- first show (Pcre2.match regex value)
            return $! split match 0 value
  where
    split :: [Pcre2.Match] -> Int -> T.Text -> [T.Text]
    split [] !_offset remainder = [remainder]
    split (Pcre2.Match (Pcre2.Range start len) _ : matches) !offset remainder =
        let (pre, post) = T.splitAt (start - offset) remainder in
        pre : split matches (start + len) (T.drop len post)

builtin_re_match :: Builtin IO
builtin_re_match = Builtin
    (Ty.string 🡒 Ty.string 🡒 Ty.out Ty.boolean) $ do
    cacheRef <- newIORef HMS.empty
    pure $
        \(Cons pattern (Cons value Nil)) -> do
        errOrRegex <- liftIO $ atomicModifyIORef' cacheRef $ \cache ->
            case HMS.lookup pattern cache of
                Just errOrRegex -> return errOrRegex
                Nothing         ->
                    let errOrRegex = Pcre2.compile pattern in
                    (HMS.insert pattern errOrRegex cache, errOrRegex)

        eitherToBuiltinM $ do
            regex <- first show errOrRegex
            match <- first show (Pcre2.match regex value)
            return $! not $ null match
