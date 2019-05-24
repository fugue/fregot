{-# LANGUAGE TemplateHaskell #-}
module Fregot.Main.Repl
    ( Options
    , parseOptions

    , main
    ) where

import           Control.Lens            ((^.))
import           Control.Lens.TH         (makeLenses)
import           Control.Monad           (forM_)
import           Control.Monad.Parachute (runParachuteT)
import qualified Data.IORef              as IORef
import qualified Fregot.Error            as Error
import qualified Fregot.Find             as Find
import qualified Fregot.Interpreter      as Interpreter
import qualified Fregot.Repl             as Repl
import qualified Fregot.Sources          as Sources
import qualified Options.Applicative     as OA
import           System.Exit             (ExitCode (..))
import qualified System.IO               as IO

data Options = Options
    { _paths :: [FilePath]
    } deriving (Show)

$(makeLenses ''Options)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> (OA.many $ OA.strArgument $
            OA.metavar "PATHS" <>
            OA.help    "Rego files or directories to load into repl")

main :: Options -> IO ExitCode
main opts = do
    sources <- Sources.newHandle
    interpreter <- Interpreter.newHandle sources
    regoPaths <- Find.findRegoFiles (opts ^. paths)
    Repl.withHandle sources interpreter $ \repl -> do
        (lerrs, _) <- runParachuteT $ do
            forM_ regoPaths $ Interpreter.loadModuleOrBundle interpreter
            Interpreter.compilePackages interpreter
        sauce <- IORef.readIORef sources
        Error.hPutErrors IO.stderr sauce Error.TextFmt lerrs
        Repl.run repl
        return ExitSuccess
