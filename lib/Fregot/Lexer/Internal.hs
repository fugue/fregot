{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Fregot.Lexer.Internal
    ( -- * Lexing
      TokenParser
    , TokenStream
    , parseTokenStream

    , peekTokenStream
    , prevTokenStream

      -- * Tokens with position info
    , TokenPos (..)
    , parseTokenPos
    , prettyTokenPos

      -- * Token representation
    , Token (..)
    , parseToken
    , prettyToken

      -- * Dealing with comments
    , Comment (..)
    , initialCommentTokenStream
    , prevCommentTokenStream

    , keywords

      -- * Auxiliary parsers
    , varHeadChar
    , varTailChar
    , varText
    ) where

import           Control.Lens           ((&), (.~), (^.))
import           Control.Monad          (void)
import           Control.Monad.Identity (Identity)
import           Data.Char              (isAlpha, isDigit, isSpace)
import qualified Data.Text              as T
import qualified Data.Vector.Extended   as V
import           Fregot.Lexer.Comment
import           Fregot.Lexer.Position
import qualified Fregot.Lexer.String    as String
import qualified Fregot.PrettyPrint     as PP
import           GHC.Generics           (Generic)
import           Prelude                hiding (lex)
import           Text.Parsec            ((<|>))
import qualified Text.Parsec.Extended   as Parsec

type TokenParser a = Parsec.ParsecT T.Text () Identity a

-- | Invariant: the `Int` which represents the current index never points to a
-- `TCommentBlock`.  This is enforced by the `mkTokenStream` smart constructor.
data TokenStream = TokenStream !(V.Vector TokenPos) !Int
    deriving (Generic, Show)

instance Monad m => Parsec.Stream TokenStream m TokenPos where
    uncons = return . unconsTokenStream

instance PP.Pretty a TokenStream where
    pretty (TokenStream ts _) = PP.vcat $
        [ PP.pretty (show (sp ^. line, sp ^. column, tok))
        | TokenPos sp tok _ <- V.toList ts
        ]

mkTokenStream :: V.Vector TokenPos -> Int -> TokenStream
mkTokenStream ts i
    | Just (TokenPos _ (TCommentBlock _) _) <- ts V.!? i =
        mkTokenStream ts (i + 1)
    | otherwise = TokenStream ts i

unconsTokenStream :: TokenStream -> Maybe (TokenPos, TokenStream)
unconsTokenStream (TokenStream ts i) = case ts V.!? i of
    Nothing -> Nothing
    Just t  -> Just (t, mkTokenStream ts (i + 1))

peekTokenStream :: TokenStream -> Maybe TokenPos
peekTokenStream (TokenStream ts i) = ts V.!? i

-- | Look at the previous token in the token stream.  Like `unconsTokenStream`,
-- this does not return comments.
prevTokenStream :: TokenStream -> TokenPos
prevTokenStream (TokenStream ts idx) = go (idx - 1)
  where
    go i = case ts V.!? i of
        Just (TokenPos _ (TCommentBlock _) _) -> go (i - 1)
        Just t  -> t
        Nothing -> TokenPos (Position 1 1) TStart (Position 1 1)

-- | Checks if the very first token on the first line is a comment, and if so,
-- return it.
initialCommentTokenStream :: TokenStream -> Maybe Comment
initialCommentTokenStream (TokenStream ts _) = case ts V.!? 0 of
    Just (TokenPos s (TCommentBlock cb) _) | s ^. line <= 1 -> Just cb
    _                                                       -> Nothing

-- | Checks if the previous token was a comment, and if so, return it along
-- with the end line number of the comment.
prevCommentTokenStream :: TokenStream -> Maybe (Comment, Int)
prevCommentTokenStream (TokenStream ts i) = case ts V.!? j of
    Just (TokenPos _ (TCommentBlock cb) e) -> Just (cb, e ^. line)
    _                                      -> Nothing
  where
    !j = i - 1

parseTokenStream :: TokenParser TokenStream
parseTokenStream = do
    Parsec.skipMany $ Parsec.eraseExpected Parsec.space
    tokens <- Parsec.many parseTokenPos
    Parsec.eof
    return $! mkTokenStream (V.fromList tokens) 0

data TokenPos = TokenPos
    {-# UNPACK #-} !Position
                   !Token
    {-# UNPACK #-} !Position
    deriving (Generic, Show)

parseTokenPos :: TokenParser TokenPos
parseTokenPos = do
    !start <- getPosition
    !tok   <- parseToken
    !end   <- getPosition
    Parsec.skipMany $ Parsec.eraseExpected Parsec.space
    return $! TokenPos start tok $ end & column .~ (end ^. column - 1)

prettyTokenPos :: TokenPos -> String
prettyTokenPos (TokenPos _ tok _) = prettyToken tok

data Token
    = TStart
    -- | These comment blocks are special in that they are never returned the
    -- normal way when parsing, the user has to explicitly ask for them.
    | TCommentBlock     !Comment
    | TString           !T.Text
    | TRawString        !T.Text
    | TInt              !Integer
    | TFloat            !Double
    | TVar              !T.Text
    | TLParen
    | TRParen
    | TLBracket
    | TRBracket
    | TLBrace
    | TRBrace
    | TUnify
    | TAssign
    | TPeriod
    | TColon
    | TSemicolon
    | TComma
    | TPipe
    | TAs
    | TDefault
    | TElse
    | TFalse
    | TImport
    | TIn
    | TPackage
    | TNot
    | TNull
    | TSome
    | TTrue
    | TWith
    | TEqual
    | TNotEqual
    | TLessThan
    | TLessThanOrEqual
    | TGreaterThan
    | TGreaterThanOrEqual
    | TPlus
    | TMinus
    | TTimes
    | TDivide
    | TModulo
    | TBinAnd
    deriving (Eq, Show, Generic)

parseToken :: TokenParser Token
parseToken =
    (TString           <$> String.string)     <|>
    (TRawString        <$> parseRawString)    <|>
    (TCommentBlock     <$> parseCommentBlock) <|>
    parseVar                                  <|>
    parseIntOrFloat                           <|>
    parseSomeOperator

parseRawString :: TokenParser T.Text
parseRawString = do
    void (Parsec.char '`')
    str <- Parsec.manyTill Parsec.anyChar (Parsec.char '`')
    return $! T.pack str

-- | A comment block
parseCommentBlock
    :: TokenParser Comment
parseCommentBlock =
    Comment <$> Parsec.many1 commentLine
  where
    commentLine :: TokenParser T.Text
    commentLine = do
        -- Skip the '# '
        void $ Parsec.char '#'
        Parsec.optional horizontalSpace

        -- Actually parse the comment
        str <- Parsec.manyTill Parsec.anyChar
            (void Parsec.endOfLine <|> Parsec.eof)

        -- Skip spaces and tabs but not newlines
        Parsec.skipMany horizontalSpace
        return $! T.pack str

    horizontalSpace :: TokenParser Char
    horizontalSpace = Parsec.satisfy $ \c -> isSpace c && c /= '\n' && c /= '\r'

parseVar :: TokenParser Token
parseVar = do
    x0 <- Parsec.satisfy varHeadChar
    xs <- Parsec.many $ Parsec.satisfy varTailChar
    let txt = T.pack $ x0 : xs
    return $ case txt of
        "as"      -> TAs
        "default" -> TDefault
        "else"    -> TElse
        "false"   -> TFalse
        "import"  -> TImport
        "in"      -> TIn
        "package" -> TPackage
        "not"     -> TNot
        "null"    -> TNull
        "some"    -> TSome
        "true"    -> TTrue
        "with"    -> TWith
        _         -> TVar txt

varHeadChar :: Char -> Bool
varHeadChar c = c == '_' || isAlpha c

varTailChar :: Char -> Bool
varTailChar c = varHeadChar c || isDigit c

varText :: T.Text -> Bool
varText "_" = False
varText v   = case T.uncons v of
    Nothing     -> False
    Just (h, t) -> varHeadChar h && T.all varTailChar t

-- | A token parser for an integer or a float.
parseIntOrFloat :: TokenParser Token
parseIntOrFloat = do
    intPart <- Parsec.many1 Parsec.digit
    floatPart <- Parsec.optionMaybe $
        Parsec.char '.' *> Parsec.many Parsec.digit
    return $ case floatPart of
        Nothing -> TInt  $ read intPart
        Just "" -> TFloat $ read intPart
        Just f  -> TFloat $ read $ intPart ++ "." ++ f

keywords :: [T.Text]
keywords =
    [ "as"
    , "default"
    , "else"
    , "false"
    , "import"
    , "package"
    , "not"
    , "null"
    , "some"
    , "true"
    , "with"
    ]

parseSomeOperator :: TokenParser Token
parseSomeOperator = do
    !spos <- Parsec.getPosition
    !x0   <- Parsec.anyChar
    case x0 of
        '(' -> return TLParen
        ')' -> return TRParen
        '{' -> return TLBrace
        '}' -> return TRBrace
        '[' -> return TLBracket
        ']' -> return TRBracket
        '.' -> return TPeriod
        ';' -> return TSemicolon
        ',' -> return TComma
        '|' -> return TPipe
        '+' -> return TPlus
        '-' -> return TMinus
        '*' -> return TTimes
        '/' -> return TDivide
        '%' -> return TModulo
        '&' -> return TBinAnd
        ':' -> do
            n <- Parsec.lookAhead Parsec.anyChar
            case n of
                '=' -> Parsec.anyChar *> return TAssign
                _   -> return TColon
        '=' -> do
            n <- Parsec.lookAhead Parsec.anyChar
            case n of
                '=' -> Parsec.anyChar *> return TEqual
                _   -> return TUnify
        '!' -> do
            n <- Parsec.lookAhead Parsec.anyChar
            case n of
                '=' -> Parsec.anyChar *> return TNotEqual
                _   -> Parsec.unexpectedAt spos ("character " ++ show x0)
        '<' -> do
            n <- Parsec.lookAhead Parsec.anyChar
            case n of
                '=' -> Parsec.anyChar *> return TLessThanOrEqual
                _   -> return TLessThan
        '>' -> do
            n <- Parsec.lookAhead Parsec.anyChar
            case n of
                '=' -> Parsec.anyChar *> return TGreaterThanOrEqual
                _   -> return TGreaterThan
        _   -> Parsec.unexpectedAt spos ("character " ++ show x0)

prettyToken :: Token -> String
prettyToken token = case token of
    TStart                 -> "start of file"
    TCommentBlock        _ -> "comment block"
    TString              _ -> "String literal"
    TRawString           _ -> "Raw string literal"
    TInt                 _ -> "Int literal"
    TFloat               _ -> "Float literal"
    TVar                 i -> T.unpack i
    TLParen                -> quote "("
    TRParen                -> quote ")"
    TLBrace                -> quote "{"
    TRBrace                -> quote "}"
    TLBracket              -> quote "["
    TRBracket              -> quote "]"
    TUnify                 -> "="
    TAssign                -> ":="
    TPeriod                -> "."
    TColon                 -> ":"
    TSemicolon             -> ";"
    TComma                 -> ","
    TPipe                  -> "|"
    TAs                    -> "keyword 'as'"
    TDefault               -> "keyword 'default'"
    TElse                  -> "keyword 'else'"
    TFalse                 -> "keyword 'false'"
    TImport                -> "keyword 'import'"
    TIn                    -> "keyword 'in'"
    TNot                   -> "keyword 'not'"
    TNull                  -> "keyword 'null'"
    TPackage               -> "keyword 'package'"
    TSome                  -> "keyword 'some'"
    TTrue                  -> "keyword 'true'"
    TWith                  -> "keyword 'with'"
    TEqual                 -> "=="
    TNotEqual              -> "!="
    TLessThan              -> "<"
    TLessThanOrEqual       -> "<="
    TGreaterThan           -> ">"
    TGreaterThanOrEqual    -> ">="
    TPlus                  -> "+"
    TMinus                 -> "-"
    TTimes                 -> "*"
    TDivide                -> "/"
    TModulo                -> "%"
    TBinAnd                -> "&"
  where
    quote :: String -> String
    quote x = "\"" ++ x ++ "\""
