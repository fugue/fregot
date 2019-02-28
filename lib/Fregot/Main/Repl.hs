module Fregot.Main.Repl
    ( Options
    , parseOptions

    , main
    ) where

import qualified Fregot.Interpreter  as Interpreter
import qualified Fregot.Repl         as Repl
import qualified Fregot.Sources      as Sources
import qualified Options.Applicative as OA

type Options = ()

parseOptions :: OA.Parser Options
parseOptions = pure ()

main :: Options -> IO ()
main () = do
    sources <- Sources.newHandle
    interpreter <- Interpreter.newHandle sources
    Repl.withHandle sources interpreter $ \repl ->
        Repl.run repl
