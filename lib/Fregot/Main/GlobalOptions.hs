{-# LANGUAGE TemplateHaskell #-}
module Fregot.Main.GlobalOptions
    ( Format (..)

    , GlobalOptions (..), dumpTags, format
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

data GlobalOptions = GlobalOptions
    { _dumpTags :: Dump.Tags
    , _format   :: Format
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

inputPath :: OA.Parser (Maybe FilePath)
inputPath = OA.optional $ OA.strOption $
    OA.metavar "PATH" <>
    OA.long    "input" <>
    OA.short   'i' <>
    OA.help    "Input filepath"
