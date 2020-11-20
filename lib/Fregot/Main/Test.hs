{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Main.Test
    ( Options
    , parseOptions

    , main
    ) where

import           Control.Lens              ((&), (.~), (^.))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad.Extended    (foldMapM, forM_)
import qualified Control.Monad.Parachute   as Parachute
import           Data.Bifunctor            (bimap)
import qualified Data.IORef                as IORef
import           Data.List                 (sortOn)
import qualified Fregot.Error              as Error
import qualified Fregot.Find               as Find
import qualified Fregot.Interpreter        as Interpreter
import           Fregot.Main.GlobalOptions
import           Fregot.Names
import qualified Fregot.Parser             as Parser
import qualified Fregot.Sources            as Sources
import           Fregot.Test
import qualified Options.Applicative       as OA
import           System.Exit               (ExitCode (..))
import qualified System.IO                 as IO

data Options = Options
    { _paths :: [DestinationPrefix FilePath]
    } deriving (Show)

$(makeLenses ''Options)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> (OA.some $ fmap parseDestinationPrefix $ OA.strArgument $
            OA.metavar "PATHS" <>
            OA.help    "Rego files or directories to test")

main :: GlobalOptions -> Options -> IO ExitCode
main gopts opts = do
    sources <- Sources.newHandle
    interpreter <- Interpreter.newHandle
        (Interpreter.defaultConfig
            & Interpreter.dumpTags .~ gopts ^. dumpTags
            & Interpreter.strictBuiltinErrors .~ gopts ^. strictBuiltinErrors)
        sources
    regoPaths <- Find.findPrefixedRegoFiles (opts ^. paths)
    (errors, mbResult) <- Parachute.runParachuteT $ do
        forM_ regoPaths $ Interpreter.loadFileByExtension
            interpreter Parser.defaultParserOptions
        Interpreter.compileRules interpreter
        tests <- sortOn (bimap unPackageName unVar) . filter isTest <$>
            Interpreter.readAllRules interpreter
        foldMapM (\t -> runTest interpreter t) tests

    sources' <- IORef.readIORef sources
    forM_ mbResult (printTestResults IO.stdout sources')
    Error.hPutErrors IO.stderr sources' (gopts ^. format) errors

    return $! case mbResult of
        _ | Error.severe errors                               -> ExitFailure 1
        Just tr | null (tr ^. failed) && null (tr ^. errored) -> ExitSuccess
        _                                                     -> ExitFailure 1
