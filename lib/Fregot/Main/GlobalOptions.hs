{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE TemplateHaskell #-}
module Fregot.Main.GlobalOptions
    ( Format (..)

    , Verbosity
    , silentVerbosity
    , defaultVerbosity

    , GlobalOptions (..), dumpTags, format, verbosity
    , parseGlobalOptions

    , inputPath
    ) where

import           Control.Lens.TH              (makeLenses)
import qualified Data.HashSet                 as HS
import qualified Fregot.Dump                  as Dump
import qualified Options.Applicative.Extended as OA

data Format
    = Text
    | Json
    deriving (Bounded, Enum, Show)

newtype Verbosity = Verbosity Int deriving (Eq, Ord, Read, Show)

silentVerbosity :: Verbosity
silentVerbosity = Verbosity 0

defaultVerbosity :: Verbosity
defaultVerbosity = Verbosity 1

data GlobalOptions = GlobalOptions
    { _dumpTags  :: Dump.Tags
    , _format    :: Format
    , _verbosity :: Verbosity
    } deriving (Show)

$(makeLenses ''GlobalOptions)

parseDumpTags :: OA.Parser Dump.Tags
parseDumpTags =
    fmap (Dump.Tags . HS.fromList) $ OA.many $ fmap Dump.Tag $ OA.strOption $
    OA.hidden <>
    OA.long    "dump" <>
    OA.metavar "TAG" <>
    OA.help    "Dump debug information"

parseGlobalOptions :: OA.Parser GlobalOptions
parseGlobalOptions = GlobalOptions
    <$> parseDumpTags
    <*> OA.plainEnumOption (
            OA.value Text <>
            OA.long "format" <>
            OA.metavar "FORMAT" <>
            OA.help "Format for error messages and diagnostics" <>
            OA.hidden)
    <*> OA.option (Verbosity <$> OA.auto) (
            OA.value defaultVerbosity <>
            OA.short 'v' <>
            OA.long "verbosity" <>
            OA.metavar "VERBOSITY" <>
            OA.hidden)

inputPath :: OA.Parser (Maybe FilePath)
inputPath = OA.optional $ OA.strOption $
    OA.metavar "PATH" <>
    OA.long    "input" <>
    OA.short   'i' <>
    OA.help    "Input filepath"
