{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
module Fregot.Main
    ( main
    ) where

import           Data.Version                 (showVersion)
import qualified Fregot.Main.Bundle           as Main.Bundle
import qualified Fregot.Main.Eval             as Main.Eval
import           Fregot.Main.GlobalOptions
import qualified Fregot.Main.Repl             as Main.Repl
import qualified Fregot.Main.Test             as Main.Test
import           Fregot.Version               (version)
import qualified Options.Applicative.Extended as OA
import           System.Exit                  (exitWith)

data Command
    = Test Main.Test.Options
    | Repl Main.Repl.Options
    | Bundle Main.Bundle.Options
    | Eval Main.Eval.Options
    deriving (Show)

type Options = (GlobalOptions, Command)

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
parseOptions = (,) <$> parseGlobalOptions <*> parseCommand

parseOptionsInfo :: OA.ParserInfo Options
parseOptionsInfo = OA.info (OA.helper <*> parseOptions) $
    OA.fullDesc <> OA.header ("fregot v" <> showVersion version)

parseOptionsPrefs :: OA.ParserPrefs
parseOptionsPrefs = OA.prefs OA.showHelpOnError

main :: IO ()
main = do
    (goptions, cmd) <- OA.customExecParser parseOptionsPrefs parseOptionsInfo
    exitWith =<< case cmd of
        Repl   o -> Main.Repl.main   goptions o
        Test   o -> Main.Test.main   goptions o
        Bundle o -> Main.Bundle.main goptions o
        Eval   o -> Main.Eval.main   goptions o
