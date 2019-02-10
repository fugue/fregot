-- | This module parses the lower-level tokens on which the rego language
-- is built.
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Fregot.Parser.Token
    ( Lex.Token (..)
    , symbol
    , anyToken

    , int
    , intOrFloat
    , boolean
    , string
    , var
    ) where

import           Data.Text    (Text)
import qualified Fregot.Lexer as Lex
import           Text.Parsec  (ParsecT, (<?>))

-- | Parse an exact token
symbol :: Monad m => Lex.Token -> ParsecT Lex.TokenStream u m ()
symbol tok0 = Lex.primToken (\tok1 -> if tok0 == tok1 then Just () else Nothing)
{-# INLINE symbol #-}

anyToken :: Monad m => ParsecT Lex.TokenStream u m ()
anyToken = Lex.primToken (\_ -> Just ())

int :: Monad m => ParsecT Lex.TokenStream u m Integer
int = Lex.primToken $ \case
    Lex.TInt i -> Just i
    _          -> Nothing

-- | A token parser for an integer or a float.
--
-- NOTE (jaspervdj): This does not work for negative numbers.
intOrFloat :: Monad m => ParsecT Lex.TokenStream u m (Either Integer Double)
intOrFloat = (<?> "int or float") $ Lex.primToken $ \case
    Lex.TInt   i -> Just (Left i)
    Lex.TFloat f -> Just (Right f)
    _            -> Nothing

-- | A token parser for a boolean.
boolean :: Monad m => ParsecT Lex.TokenStream u m Bool
boolean = (<?> "boolean") $ Lex.primToken $ \case
    Lex.TTrue  -> Just True
    Lex.TFalse -> Just False
    _          -> Nothing

-- | A string literal.
string :: Monad m => ParsecT Lex.TokenStream u m Text
string = (<?> "string literal") $ Lex.primToken $ \case
    Lex.TString s -> Just s
    _             -> Nothing

-- | An identifier
var :: Monad m => ParsecT Lex.TokenStream u m Text
var = (<?> "var") $ Lex.primToken $ \case
    Lex.TVar t -> Just t
    _          -> Nothing
