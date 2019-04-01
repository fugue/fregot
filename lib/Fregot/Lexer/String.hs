-- | TODO: Make this cover rego string literals rather than Ludwig ones.
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Fregot.Lexer.String
    ( string
    ) where

import           Control.Applicative    ((<|>))
import           Control.Monad          (replicateM)
import           Data.Char              (chr, isHexDigit)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Numeric                (readHex)
import           Text.Parsec            (ParsecT, many, satisfy)
import           Text.Parsec.Char       (char)
import           Text.Parsec.Combinator (between)
import           Text.Parsec.Prim       (Stream, (<?>))

-- | Ludwig string literals. Strings can be either single- or double-quoted.
--
-- Escape codes are based on JSON string literals, see
-- <http://www.json.org/string.gif>.

-- Strings can span multiple lines. In that case, new lines should start with
-- a `\` character. E.g.:
--
-- > long-string:
-- >   "Hello world
-- >   \Second line!"
--

string
    :: Stream s m Char => ParsecT s u m Text
string = do
    str <- stringQuoted '"' <|> stringQuoted '\''
    return $! T.pack str
  where
    stringQuoted end = between
        (char end)
        (char end <?> "end of string")
        (many $ stringChar end)

    stringChar end =
        stringLetter end <|> stringEscape <?> "string character"

    stringLetter end =
        satisfy (\c -> (c /= end) && (c /= '\\') && (c > '\026'))

    stringEscape = do
        _ <- char '\\'
        esc <- escapeCode
        return esc

    escapeCode =
        (char '"')  <|>
        (char '\'') <|>
        (char '\\') <|>
        (char '/')  <|>
        (char 'b' >> return '\b') <|>
        (char 'f' >> return '\f') <|>
        (char 'n' >> return '\n') <|>
        (char 'r' >> return '\r') <|>
        (char 't' >> return '\t') <|>
        (char 'u' >> hexadecimal)

    hexadecimal = do
        chars <- replicateM 4 (satisfy isHexDigit)
        case [h | (h, r) <- readHex chars, null r] of
            []      -> fail "hexadecimal"
            (h : _) -> return $ chr h
