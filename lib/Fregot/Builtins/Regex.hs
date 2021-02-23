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
    [ (NamedFunction (BuiltinName "re_match"),                           builtin_regex_match)
    , (NamedFunction (QualifiedName "regex.find_n"),                     builtin_regex_find_n)
    , (NamedFunction (QualifiedName "regex.find_all_string_submatch_n"), builtin_regex_find_all_string_submatch_n)
    , (NamedFunction (QualifiedName "regex.is_valid"),                   builtin_regex_is_valid)
    , (NamedFunction (QualifiedName "regex.match"),                      builtin_regex_match)
    , (NamedFunction (QualifiedName "regex.split"),                      builtin_regex_split)
    ]

-- | Utility method for builtins taking a pattern and an input string.
makeRegexBuiltin
    :: ToVal o
    => Ty.BuiltinType i o
    -> ([Pcre2.Match] -> T.Text -> Args i -> o) -> Builtin IO
makeRegexBuiltin sig f = Builtin
    (Ty.string 🡒 Ty.string 🡒 sig) $ do
    cacheRef <- newIORef HMS.empty
    pure $ \(Cons needle (Cons haystack args)) -> do
        errOrRegex <- liftIO $ atomicModifyIORef' cacheRef $ \cache ->
            case HMS.lookup needle cache of
                Just errOrRegex -> (cache, errOrRegex)
                Nothing         ->
                    let errOrRegex = Pcre2.compile needle in
                    (HMS.insert needle errOrRegex cache, errOrRegex)
        eitherToBuiltinM $ do
            regex <- first show errOrRegex
            matches <- first show $ Pcre2.match regex haystack
            pure $ f matches haystack args

extractMatches :: [Pcre2.Match] -> T.Text -> [(T.Text, [T.Text])]
extractMatches = go 0
  where
    go _ [] _ = []
    go !offset (Pcre2.Match (Pcre2.Range start len) subs : matches) remainder =
        let post = T.drop (start - offset) remainder
            subs' = do
                Pcre2.Range subStart subLen <- subs
                pure $ T.take subLen $ T.drop (subStart - start) post in
        (T.take len post, subs') : go start matches post

builtin_regex_find_n :: Builtin IO
builtin_regex_find_n = makeRegexBuiltin
    (Ty.number 🡒 Ty.out (Ty.arrayOf Ty.string)) $
    \matches haystack (Cons number Nil) ->
        let texts = map fst $ extractMatches matches haystack in
        if number < 0 then texts else take number texts

builtin_regex_find_all_string_submatch_n :: Builtin IO
builtin_regex_find_all_string_submatch_n = makeRegexBuiltin
    (Ty.number 🡒 Ty.out (Ty.arrayOf (Ty.arrayOf Ty.string))) $
    \matches haystack (Cons number Nil) ->
        let texts = map (uncurry (:)) $ extractMatches matches haystack in
        if number < 0 then texts else take number texts

builtin_regex_is_valid :: Monad m => Builtin m
builtin_regex_is_valid = Builtin
    (Ty.string 🡒 Ty.out Ty.boolean) $ pure $
    \(Cons pattern Nil) -> case Pcre2.compile pattern of
        Left  _ -> pure False
        Right _ -> pure True

builtin_regex_match :: Builtin IO
builtin_regex_match = makeRegexBuiltin
    (Ty.out Ty.boolean) $
    \matches _ _ -> not $ null matches

builtin_regex_split :: Builtin IO
builtin_regex_split = makeRegexBuiltin
    (Ty.out (Ty.arrayOf Ty.string)) $
    \matches haystack Nil -> split matches 0 haystack
  where
    split :: [Pcre2.Match] -> Int -> T.Text -> [T.Text]
    split [] !_offset remainder = [remainder]
    split (Pcre2.Match (Pcre2.Range start len) _ : matches) !offset remainder =
        let (pre, post) = T.splitAt (start - offset) remainder in
        pre : split matches (start + len) (T.drop len post)
