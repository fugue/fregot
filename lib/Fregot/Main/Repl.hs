{-# LANGUAGE TemplateHaskell #-}
module Fregot.Main.Repl
    ( Options
    , parseOptions

    , main
    ) where

import           Control.Lens              ((^.))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad             (forM_)
import           Control.Monad.Parachute   (runParachuteT)
import           Control.Monad.Trans       (liftIO)
import qualified Data.IORef                as IORef
import qualified Fregot.Error              as Error
import qualified Fregot.Find               as Find
import qualified Fregot.Interpreter        as Interpreter
import           Fregot.Main.GlobalOptions
import           Fregot.Names
import           Fregot.Parser             (defaultParserOptions)
import qualified Fregot.Repl               as Repl
import qualified Fregot.Repl.FileWatch     as FileWatch
import qualified Fregot.Sources            as Sources
import qualified Options.Applicative       as OA
import           System.Exit               (ExitCode (..))
import qualified System.IO                 as IO

data Options = Options
    { _input         :: Maybe FilePath
    , _watch         :: Bool
    , _noHistoryFile :: Bool
    , _opt           :: Bool
    , _paths         :: [DestinationPrefix FilePath]
    } deriving (Show)

$(makeLenses ''Options)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> inputPath
    <*> (OA.switch $
            OA.long  "watch" <>
            OA.short 'w' <>
            OA.help  "Watch files for changes and reload automatically")
    <*> (OA.switch $
            OA.long "no-history-file" <>
            OA.hidden <>
            OA.help "Don't save repl history to a file")
    <*> (OA.switch $
            OA.short 'O' <>
            OA.hidden <>
            OA.help "Turn on optimizations when debugging")
    <*> (OA.many $ fmap parseDestinationPrefix $ OA.strArgument $
            OA.metavar "PATHS" <>
            OA.help    "Rego files or directories to load into repl")

main :: GlobalOptions -> Options -> IO ExitCode
main gopts opts = do
    sources     <- Sources.newHandle
    itpr        <- Interpreter.newHandle
        (Interpreter.defaultConfig
            { Interpreter._opt      = opts ^. opt
            , Interpreter._dumpTags = gopts ^. dumpTags
            })
        sources
    regoPaths   <- Find.findPrefixedRegoFiles (opts ^. paths)

    replConfig <-
        (\c -> if opts ^. noHistoryFile
            then c {Repl._historyFile = Nothing}
            else c) .
        (\c -> c {Repl._verbosity = gopts ^. verbosity}) <$>
        Repl.defaultConfig

    FileWatch.withHandle (FileWatch.Config (opts ^. watch)) $ \fileWatch ->
        Repl.withHandle replConfig sources fileWatch itpr $ \repl -> do
        (lerrs, mbResult) <- runParachuteT $ do
            forM_ (opts ^. input) $ liftIO . Repl.setInputFile repl
            forM_ regoPaths $ \path@(DestinationPrefix pkg fp) -> do
                liftIO $ FileWatch.watch fileWatch fp pkg
                Interpreter.loadFileByExtension itpr defaultParserOptions path
            Interpreter.compileRules itpr
        sauce <- IORef.readIORef sources
        Error.hPutErrors IO.stderr sauce Error.Text lerrs
        case mbResult of
            Nothing -> return (ExitFailure 1)
            Just _  -> do
                Repl.run repl
                return ExitSuccess
