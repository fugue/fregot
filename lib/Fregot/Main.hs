{-# LANGUAGE TemplateHaskell #-}
module Fregot.Main
    ( Options, verbosity, command
    , main
    ) where

import           Control.Lens        ((^.))
import           Control.Lens.TH     (makeLenses)
import           Data.Version        (showVersion)
import qualified Fregot.Main.Repl    as Main.Repl
import qualified Fregot.Main.Test    as Main.Test
import qualified Options.Applicative as OA
import qualified Paths_fregot
import           System.Exit         (exitWith)

data Command
    = Test Main.Test.Options
    | Repl Main.Repl.Options
    deriving (Show)

data Options = Options
    { _verbosity :: ()  -- Placeholder
    , _command   :: Command
    } deriving (Show)

$(makeLenses ''Options)

parseCommand :: OA.Parser Command
parseCommand = OA.subparser $ mconcat
    [ cmd "repl" Repl Main.Repl.parseOptions "Run fregot repl"
    , cmd "test" Test Main.Test.parseOptions "Run tests in .rego files"
    ]
  where
    cmd name con p descr =
        OA.command name (OA.info (con <$> p) (OA.progDesc descr))

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> pure ()
    <*> parseCommand

parseOptionsInfo :: OA.ParserInfo Options
parseOptionsInfo = OA.info (OA.helper <*> parseOptions) $
    OA.fullDesc <>
    OA.header ("fregot v" <> showVersion Paths_fregot.version)

parseOptionsPrefs :: OA.ParserPrefs
parseOptionsPrefs = OA.prefs OA.showHelpOnError

main :: IO ()
main = do
    options <- OA.customExecParser parseOptionsPrefs parseOptionsInfo
    exitWith =<< case options ^. command of
        Repl o -> Main.Repl.main o
        Test o -> Main.Test.main o
