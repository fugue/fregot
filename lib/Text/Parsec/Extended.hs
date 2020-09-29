{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Parsec.Extended
    ( module Text.Parsec
    , unexpectedAt
    , throwParseError
    , eraseExpected
    , recover
    ) where

import           Text.Parsec
import           Text.Parsec.Error

-- | Like 'unexpected', but allows a custom error position.
unexpectedAt :: Stream s m t => SourcePos -> String -> ParsecT s u m a
unexpectedAt pos msg = throwParseError $ newErrorMessage (UnExpect msg) pos

-- | This function allows you to throw a 'ParseError' with a custom error.
throwParseError :: Stream s m t => ParseError -> ParsecT s u m a
throwParseError err = mkPT $ \_ -> return $ Consumed $ return $ Error err

-- | This function erases the associated "expected ..." message of a parser.
-- This is useful when that message is irrelevant to the user.
eraseExpected :: Stream s m t => ParsecT s u m a -> ParsecT s u m a
eraseExpected = (<?> "")

-- | Allow recovering from a parser that fails /after consuming some input/.
recover
    :: Stream s m t
    => (ParseError -> r -> a)           -- ^ Error handler
    -> (ParseError -> ParsecT s u m r)  -- ^ Recovering parser
    -> ParsecT s u m a                  -- ^ Parser to recover from
    -> ParsecT s u m a                  -- ^ Resulting parser
recover handler recoveryParser parser = mkPT $ \state0 -> do
    consumed <- runParsecT parser state0
    case consumed of
        Empty    mReply -> return (Empty mReply)
        Consumed mReply -> do
            reply <- mReply
            case reply of
                Ok _ _ _  -> return (Consumed (return reply))
                Error err -> startRecovery state0 reply err
  where
    startRecovery state0 originalReply err = do
        consumed <- runParsecT (recoveryParser err) state0
        return $ flip fmap consumed $ \mReply -> do
            reply <- mReply
            case reply of
                Ok x state1 _ -> return (Ok (handler err x) state1 err)
                Error _       -> return originalReply
