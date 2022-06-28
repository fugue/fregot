{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Builtins.RegexGlobsMatch
    ( builtins
    ) where

import qualified Data.GSet                as ReSet
import qualified Data.HashMap.Strict      as HMS
import           Data.List                (foldl')
import           Data.Semiring            (Semiring (..))
import qualified Data.Text                as T
import           Fregot.Builtins.Internal
import           Fregot.Names
import           Fregot.Types.Builtins    ((🡒))
import qualified Fregot.Types.Builtins    as Ty
import qualified Fregot.Types.Internal    as Ty
import qualified RegExp.Derivative        as Re
import qualified RegExp.Operations        as Re
import qualified RegExp.RegExp            as Re

builtins :: Monad m => Builtins m
builtins = HMS.fromList
    [ (NamedFunction (QualifiedName "regex.globs_match"), builtin_regex_globs_match)
    ]

builtin_regex_globs_match :: Monad m => Builtin m
builtin_regex_globs_match = Builtin
    (Ty.string 🡒 Ty.string 🡒 Ty.out Ty.boolean) $ pure $ do
    \(Cons l (Cons r Nil)) -> eitherToBuiltinM $
        regexGlobsMatch (T.unpack l) (T.unpack r)

data Expression a
    = Dot
    | Char a
    | Plus (Expression a)
    | Star (Expression a)
    | Class [Either Char (Char, Char)]
    deriving (Show)

parse :: String -> Either String [Expression Char]
parse = expr []
  where
    expr stack = \case
        [] -> pure $ reverse stack
        '\\' : [] -> Left "unexpected \\ at end"
        '\\' : c : t -> expr (Char c : stack) t
        '*' : t -> case stack of
            e : es -> expr (Star e : es) t
            []     -> Left "unexpected *"
        '+' : t -> case stack of
            e : es -> expr (Plus e : es) t
            []     -> Left "unexpected +"
        '.' : t -> expr (Dot : stack) t
        '[' : t -> do
            (c, t') <- classes [] t
            expr (c : stack) t'
        x : t -> expr (Char x : stack) t

    classes acc = \case
         []                            -> Left "expected ]"
         ']' :                       t -> Right (Class acc, t)
         '\\' : x : '-' : '\\' : y : t -> classes (Right (x, y) : acc) t
         x        : '-' : '\\' : y : t -> classes (Right (x, y) : acc) t
         '\\' : x : '-' :        y : t -> classes (Right (x, y) : acc) t
         '\\' : x :                  t -> classes (Left x : acc) t
         x        : '-' :        y : t -> classes (Right (x, y) : acc) t
         x    :                      t -> classes (Left x : acc) t

toRe :: [Expression Char] -> Re.RegExp Char
toRe = foldl' Re.rTimes Re.rOne . map fromExpr
  where
    fromExpr :: Expression Char -> Re.RegExp Char
    fromExpr = \case
        Dot      -> Re.rLiteral one
        Char x   -> Re.rLiteral $ ReSet.singleton x
        Plus e   -> let r = fromExpr e in r `Re.rTimes` Re.rStar r
        Star e   -> Re.rStar (fromExpr e)
        Class cs -> Re.rLiteral . sconcat $ map fromClass cs

    fromClass = \case
        Left  x      -> ReSet.singleton x
        Right (x, y) -> sconcat [ReSet.singleton c | c <- [x .. y]]

    sconcat = foldl' (<+>) zero

regexGlobsMatch :: String -> String -> Either String Bool
regexGlobsMatch str1 str2 = do
    re1 <- toRe <$> parse str1
    re2 <- toRe <$> parse str2
    case Re.equivalent (Re.intersection re1 re2) Re.rZero of
        Left  _ -> pure True
        Right _ -> pure False
