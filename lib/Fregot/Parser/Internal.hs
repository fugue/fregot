{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

This module provides the types and low-level bits for parsing source code.
-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Fregot.Parser.Internal
    ( -- * Types
      FregotState (..)
    , FregotParser

      -- * Running the parsers
    , parse
    , lexAndParse

      -- * Metadatathings
    , getSourceSpan
    , updateSourceSpanWithEnd
    , withSourceSpan

      -- * Comments
    , Lex.Comment (..)
    , Lex.getInitialComment
    , getAttachedComment

      -- * Error recovery
    , tellError
    , bracketRecover
    , recover
    , expectToken
    , expectNode
    , ifErrors
    ) where

import           Control.Applicative       (empty)
import           Control.Lens              ((&), (.~), (^.))
import           Control.Monad.Identity    (when)
import           Control.Monad.Parachute   (ParachuteT)
import qualified Control.Monad.Parachute   as Parachute
import           Control.Monad.Reader      (Reader, ask, local, runReader)
import qualified Data.Text                 as T
import qualified Fregot.Error              as Error
import qualified Fregot.Lexer              as Lex
import qualified Fregot.Lexer.Position     as Lex
import qualified Fregot.Parser.Token       as Tok
import qualified Fregot.Sources            as Sources
import           Fregot.Sources.SourceSpan
import qualified Text.Parsec.Error         as Parsec
import           Text.Parsec.Extended      ((<|>))
import qualified Text.Parsec.Extended      as Parsec

-- | The userstate we use for the 'ParsecT' monad.
data FregotState = FregotState
    { sErrors        :: ![Error.Error]
    , sSourcePointer :: !Sources.SourcePointer
    }

-- | We allow storing a local "end of recovery" parser.  When something goes
-- wrong, and we call "recover", this parser will be used to figure out where
-- the recovery should stop skipping over tokens and resume parsing.
data BracketRecover = BracketRecover
    -- We use a Rank-2 type to hide the `u` and `m` type variables.  That way
    -- we can't rely or mess around with `FregotState` during recovery.
    (forall u m. Monad m => Parsec.ParsecT Lex.TokenStream u m ())

-- | This parser always fails so it never resumes.
noResumeFromRecover :: BracketRecover
noResumeFromRecover = BracketRecover empty

-- | Our parser type.
type FregotParser a =
    Parsec.ParsecT Lex.TokenStream FregotState (Reader BracketRecover) a

lexAndParse
    :: Monad m
    => FregotParser a -> Sources.SourcePointer -> T.Text
    -> ParachuteT Error.Error m a
lexAndParse parser sp txt = do
    ts <- Lex.lex sp txt
    parse parser sp ts

-- | Low-level parser.
parse
    :: Monad m
    => FregotParser a -> Sources.SourcePointer -> Lex.TokenStream
    -> ParachuteT Error.Error m a
parse parser sp inputTokens =
    case errOrOk of
        Right (x, es) -> Parachute.tellErrors es >> return x
        Left err      -> Parachute.fatal $
            Error.fromParsecError' Error.FatalSeverity sp err
  where
    errOrOk = flip runReader noResumeFromRecover $!
        Parsec.runParserT
            parser' fregotState (Sources.describeSourcePointer sp) inputTokens

    fregotState = FregotState
        { sErrors      = []
        , sSourcePointer = sp
        }

    parser' = do
        x  <- Lex.setInitialPos *> parser <* Lex.eof
        ls <- Parsec.getState
        return (x, sErrors ls)

-- | Log an error or warning.
tellError :: Error.Error -> FregotParser ()
tellError e = Parsec.modifyState $ \s -> do
    s {sErrors = e : sErrors s}

-- | Update the current position in the metadata and return it.
getSourceSpan :: FregotParser SourceSpan
getSourceSpan = do
    pos   <- Lex.getPosition
    state <- Parsec.getState
    return SourceSpan
        { _sourcePointer = sSourcePointer state
        , _start         = pos
        , _end           = pos
        }

updateSourceSpanWithEnd :: SourceSpan -> FregotParser SourceSpan
updateSourceSpanWithEnd ss = do
    prevEnd <- Lex.getPrevTokenEnd
    return $! ss & end .~ prevEnd

withSourceSpan :: FregotParser (SourceSpan -> a) -> FregotParser a
withSourceSpan p = do
    !ss0 <- getSourceSpan
    !f   <- p
    !ss1 <- updateSourceSpanWithEnd ss0
    return $! f ss1

-- | Get the previous comment, if it is "attached" to the metadata.
getAttachedComment :: SourceSpan -> FregotParser (Maybe Lex.Comment)
getAttachedComment ss = do
    c <- Lex.getPrevComment
    return $! case c of
        Just (x, endLine) | endLine == (ss ^. start . line) -> Just x
        _                                                   -> Nothing

-- | Sets a parser that recognises points where we might recover from.  This
-- could be a closing brace, a new toplevel definition, a new local
-- definition...
bracketRecover
    :: (forall u m. Monad m => Parsec.ParsecT Lex.TokenStream u m ())
    -> FregotParser a
    -> FregotParser a
bracketRecover bracket = local (const $ BracketRecover bracket)

-- | This attempts to execute the given parser.  If something goes wrong /and/
-- input is consumed, we call the parser set by 'bracketRecover' to find the
-- resume point, and we use the supplied handler to create an error node.
recover
    :: (SourceSpan -> Error.Error -> a)
    -> FregotParser a
    -> FregotParser a
recover handler p = do
    state  <- Parsec.getState
    BracketRecover resume <- ask
    Parsec.recover
        (\_err0 (meta, err) -> handler meta err)
        (\err0 -> do
            sspan0 <- getSourceSpan
            _      <- Tok.anyToken
            _      <- Parsec.manyTill Tok.anyToken (resume <|> Lex.eof)
            sspan  <- updateSourceSpanWithEnd sspan0
            let err = Error.fromParsecError
                        Error.ErrorSeverity (sSourcePointer state) sspan err0
            tellError err
            return (sspan, err))
        p

-- | Expect a certain token.  If it is not found, we insert an error in the
-- parser state, but we continue parsing.  This can be useful to improve error
-- messages and recover if there's e.g. a missing closing bracket.
expectToken :: Lex.Token -> FregotParser ()
expectToken tok =
    expect
        (\_meta _err -> return ())
        (Lex.prettyToken tok)
        (Tok.symbol tok)

-- | Expect a certain type of node.  If it is not found (it fails without
-- consuming input), we construct an error node using the provided function.
--
-- We provide a detailed error message that mentions the last token.  This makes
-- thie a very good fit if you know what kind of node to expect /because/ of the
-- last token.  This is usually the case.
expectNode
    :: (SourceSpan -> Error.Error -> a)  -- ^ How to construct an error node
    -> String                            -- ^ Description of what we expect, used in error
    -> FregotParser a                    -- ^ Parser for the node
    -> FregotParser a                    -- ^ New parser
expectNode handler description p =
    expect (\meta err -> return $ handler meta err) description p

-- | Low-level function powering both 'expectToken' and 'expectNode'.
expect
    :: (SourceSpan -> Error.Error -> FregotParser a)
    -- ^ How to construct an error
    -> String
    -- ^ Description of what we expect, used in error
    -> FregotParser a
    -- ^ Parser for the node
    -> FregotParser a
    -- ^ New parser
expect handler description p =
    p <|> makeErrorNode
  where
    makeErrorNode = do
        sp0        <- Parsec.getPosition
        prevTok    <- Lex.getPrevToken
        prevTokEnd <- Lex.getPrevTokenEnd
        meta0      <- getSourceSpan
        state      <- Parsec.getState

        let meta = meta0 & start .~ prevTokEnd & end .~ prevTokEnd
            sp1  = Parsec.setSourceLine sp0 (prevTokEnd ^. line)
            sp2  = Parsec.setSourceColumn sp1 (prevTokEnd ^. column)

        let msg  = Parsec.Message $ "expected " ++ description ++
                    " after " ++ Lex.prettyTokenPos prevTok
            err0 = Parsec.newErrorMessage msg sp2
            err1 = Error.fromParsecError Error.ErrorSeverity (sSourcePointer state) meta err0

        tellError err1
        handler meta err1

-- | Execute the first parser only if there were any errors when running the
-- second parser.
ifErrors :: FregotParser () -> FregotParser a -> FregotParser a
ifErrors onErrs parser = do
    numErrs0 <- length . sErrors <$> Parsec.getState
    x        <- parser
    numErrs1 <- length . sErrors <$> Parsec.getState
    when (numErrs0 < numErrs1) onErrs
    return x
