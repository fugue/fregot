{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

This module allows you to write something like:

> 1 + (

at the REPL, at which point we'll detect that the line is not yet completed.
-}
module Fregot.Repl.Multiline
    ( Multiline (..)
    , Partial
    , emptyPartial

    , feed
    , finish
    ) where

import qualified Data.Text    as T
import Control.Monad.Identity (runIdentity)
import qualified Text.Parsec as Parsec
import qualified Fregot.Lexer.Internal as Lex

data Multiline
    = Complete T.Text  -- Might be invalid!
    | Partial Partial

type Partial = ([T.Text], [Lex.Token])

emptyPartial :: Partial
emptyPartial = ([], [])

feed :: Partial -> T.Text -> Multiline
feed (lines0, stack0) line
    | Right ts <- Parsec.parse (Lex.parseTokenStream <* Parsec.eof) "feed" line
    , Just stack1@(_ : _) <- stream stack0 ts =
        Partial (lines1, stack1)
    | otherwise =
        Complete $ finish (lines1, stack0)
  where
    lines1 = line : lines0

finish :: Partial -> T.Text
finish ([line], _) = line
finish (lines0, _) = T.unlines $ reverse lines0

stream :: [Lex.Token] -> Lex.TokenStream -> Maybe [Lex.Token]
stream stack0 tokens0 = case runIdentity (Parsec.uncons tokens0) of
    -- End of line
    Nothing -> Just stack0

    -- Add a left paren to the stack
    Just (Lex.TokenPos _ Lex.TLParen _, tokens1) ->
        stream (Lex.TLParen : stack0) tokens1
    Just (Lex.TokenPos _ Lex.TLBracket _, tokens1) ->
        stream (Lex.TLBracket : stack0) tokens1
    Just (Lex.TokenPos _ Lex.TLBrace _, tokens1) ->
        stream (Lex.TLBrace : stack0) tokens1

    -- Pop a left paren from the stack
    Just (Lex.TokenPos _ Lex.TRParen _, tokens1)
        | Lex.TLParen : stack1 <- stack0 -> stream stack1 tokens1
        | otherwise                      -> Nothing
    Just (Lex.TokenPos _ Lex.TRBracket _, tokens1)
        | Lex.TLBracket : stack1 <- stack0 -> stream stack1 tokens1
        | otherwise                        -> Nothing
    Just (Lex.TokenPos _ Lex.TRBrace _, tokens1)
        | Lex.TLBrace : stack1 <- stack0 -> stream stack1 tokens1
        | otherwise                      -> Nothing

    -- Other character
    Just (_, tokens1) -> stream stack0 tokens1
