{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Fregot.Lexer
    ( -- * Using the token parser
      lex
    , setInitialPos
    , eof

    , getPrevToken
    , getPrevTokenEnd

    , Comment (..)
    , getInitialComment
    , getPrevComment

      -- * Types
    , TokenStream
    , parseTokenStream
    , Position (..)
    , initPosition

    , TokenPos
    , prettyTokenPos
    , Token (..)
    , prettyToken

      -- * Consuming a token stream
    , primToken

      -- * Other things
    , keywords
    ) where

import           Control.Lens            ((^.))
import           Control.Monad.Parachute
import qualified Data.Text               as T
import qualified Fregot.Error            as Error
import           Fregot.Lexer.Internal
import           Fregot.Lexer.Position
import           Fregot.Sources
import           Prelude                 hiding (lex)
import qualified Text.Parsec.Extended    as Parsec

lex
    :: Monad m
    => SourcePointer -> T.Text -> ParachuteT Error.Error m TokenStream
lex sp txt =
    case Parsec.parse (parseTokenStream <* Parsec.eof) (describeSourcePointer sp) txt of
        Left err -> fatal $ Error.fromParsecError' Error.FatalSeverity sp err
        Right ts -> return ts

-- | Parsers using a token stream should call this.
setInitialPos :: Monad m => Parsec.ParsecT TokenStream u m ()
setInitialPos = do
    ts  <- Parsec.getInput
    pos <- Parsec.getPosition
    case peekTokenStream ts of
        Nothing  -> return ()
        Just tok -> Parsec.setPosition $ setSourcePosToToken tok pos

-- | Replacement for Parsec's 'eof' that gives nicer error messages.
eof :: Monad m => Parsec.ParsecT TokenStream u m ()
eof = do
    ts <- Parsec.getInput
    case peekTokenStream ts of
        Just tok -> Parsec.unexpected (prettyTokenPos tok)
        Nothing  -> return ()

-- | Get the last token that was consumed.
getPrevToken :: Monad m => Parsec.ParsecT TokenStream u m TokenPos
getPrevToken = do
    ts <- Parsec.getInput
    return $! prevTokenStream ts

-- | Get the ending position of the last token that was consumed.
getPrevTokenEnd :: Monad m => Parsec.ParsecT TokenStream u m Position
getPrevTokenEnd = (\(TokenPos _ _ end) -> end) <$> getPrevToken

getInitialComment :: Monad m => Parsec.ParsecT TokenStream u m (Maybe Comment)
getInitialComment = initialCommentTokenStream <$> Parsec.getInput

getPrevComment
    :: Monad m => Parsec.ParsecT TokenStream u m (Maybe (Comment, Int))
getPrevComment = prevCommentTokenStream <$> Parsec.getInput

-- | Use this to parse a token from a token stream.
primToken
    :: Monad m => (Token -> Maybe a) -> Parsec.ParsecT TokenStream u m a
primToken fromToken =
    Parsec.tokenPrim prettyTokenPos nextPos
        (\(TokenPos _ tok _) -> fromToken tok)
  where
    nextPos sourcePos tok ts = case peekTokenStream ts of
        Nothing -> setSourcePosToToken tok sourcePos
        Just tp -> setSourcePosToToken tp sourcePos
    {-# INLINE nextPos #-}
{-# INLINE primToken #-}

setSourcePosToToken :: TokenPos -> Parsec.SourcePos -> Parsec.SourcePos
setSourcePosToToken (TokenPos start _ _) p0 =
    let !p1 = Parsec.setSourceLine p0 (start ^. line) in
    Parsec.setSourceColumn p1 (start ^. column)
{-# INLINE setSourcePosToToken #-}
