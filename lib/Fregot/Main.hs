{-# LANGUAGE TemplateHaskell #-}
module Fregot.Main
    ( Options, verbosity, command
    , main
    ) where

import           Control.Lens        ((^.))
import           Control.Lens.TH     (makeLenses)
import           Data.Version        (showVersion)
import qualified Fregot.Main.Bundle  as Main.Bundle
import qualified Fregot.Main.Repl    as Main.Repl
import qualified Fregot.Main.Test    as Main.Test
import qualified Fregot.Main.Eval    as Main.Eval
import           Fregot.Version      (version)
import qualified Options.Applicative as OA
import           System.Exit         (exitWith)

data Command
    = Test Main.Test.Options
    | Repl Main.Repl.Options
    | Bundle Main.Bundle.Options
    | Eval Main.Eval.Options
    deriving (Show)

data Options = Options
    { _verbosity :: ()  -- Placeholder
    , _command   :: Command
    } deriving (Show)

$(makeLenses ''Options)

parseCommand :: OA.Parser Command
parseCommand = OA.subparser $ mconcat
    [ cmd "repl"   Repl   Main.Repl.parseOptions   "Run fregot repl"
    , cmd "test"   Test   Main.Test.parseOptions   "Run tests in .rego files"
    , cmd "bundle" Bundle Main.Bundle.parseOptions "Bundle .rego files"
    , cmd "eval"   Eval   Main.Eval.parseOptions   "Evaluate a rego expression"
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
    OA.fullDesc <> OA.header ("fregot v" <> showVersion version)

parseOptionsPrefs :: OA.ParserPrefs
parseOptionsPrefs = OA.prefs OA.showHelpOnError

main :: IO ()
main = do
    options <- OA.customExecParser parseOptionsPrefs parseOptionsInfo
    exitWith =<< case options ^. command of
        Repl   o -> Main.Repl.main   o
        Test   o -> Main.Test.main   o
        Bundle o -> Main.Bundle.main o
        Eval   o -> Main.Eval.main   o
