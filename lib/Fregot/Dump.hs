-- | Utility for dumping debug information to stderr.
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fregot.Dump
    ( Tag (..)
    , Tags (..)
    , MonadDump (..)
    ) where

import           Control.Monad           (when)
import           Control.Monad.Parachute (ParachuteT)
import           Control.Monad.Reader    (ReaderT, ask)
import           Control.Monad.Trans     (MonadIO, lift, liftIO)
import           Data.Hashable           (Hashable)
import qualified Data.HashSet            as HS
import           Data.String             (IsString)
import qualified Data.Text               as T
import qualified Fregot.PrettyPrint      as PP
import qualified System.IO               as IO

newtype Tag = Tag T.Text deriving (Eq, Hashable, IsString, Show)
newtype Tags = Tags (HS.HashSet Tag) deriving (Show)

class Monad m => MonadDump m where
    dump :: PP.Pretty PP.Sem a => Tag -> a -> m ()

instance MonadIO m => MonadDump (ReaderT Tags m) where
    dump tag doc = do
        Tags tags <- ask
        when (tag `HS.member` tags) $ liftIO $
            PP.hPutSemDoc IO.stderr (PP.pretty doc)

instance MonadDump m => MonadDump (ParachuteT e m) where
    dump tag = lift . dump tag
