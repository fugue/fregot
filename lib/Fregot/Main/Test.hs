{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Main.Test
    ( Options
    , parseOptions

    , main
    ) where

import           Control.Lens            ((^.))
import           Control.Lens.TH         (makeLenses)
import           Control.Monad.Extended  (foldMapM, forM_)
import qualified Control.Monad.Parachute as Parachute
import qualified Data.IORef              as IORef
import qualified Fregot.Error            as Error
import qualified Fregot.Find             as Find
import qualified Fregot.Interpreter      as Interpreter
import qualified Fregot.Sources          as Sources
import           Fregot.Test
import qualified Options.Applicative     as OA
import           System.Exit             (ExitCode (..))
import qualified System.IO               as IO

data Options = Options
    { _paths :: [FilePath]
    } deriving (Show)

$(makeLenses ''Options)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> (OA.some $ OA.strArgument $
            OA.metavar "PATHS" <>
            OA.help    "Rego files or directories to test")

main :: Options -> IO ExitCode
main opts = do
    sources <- Sources.newHandle
    interpreter <- Interpreter.newHandle sources
    regoPaths <- Find.findRegoFiles (opts ^. paths)
    (errors, mbResult) <- Parachute.runParachuteT $ do
        forM_ regoPaths $ \path -> Interpreter.loadModule interpreter path
        Interpreter.compilePackages interpreter
        tests <- filter isTest <$> Interpreter.readRules interpreter
        foldMapM (\t -> runTest interpreter t) tests

    sources' <- IORef.readIORef sources
    forM_ mbResult (printTestResults IO.stdout sources')
    Error.hPutErrors IO.stderr sources' Error.TextFmt errors

    return $! case mbResult of
        _ | Error.severe errors -> ExitFailure 1
        Just tr | null (tr ^. failed) && null (tr ^. errored) -> ExitSuccess
        _ -> ExitFailure 1
