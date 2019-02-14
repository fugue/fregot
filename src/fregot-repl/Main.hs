module Main where

import qualified Fregot.Repl as Repl
import qualified Fregot.Sources as Sources
import qualified Fregot.Interpreter as Interpreter

main :: IO ()
main = do
    sources <- Sources.newHandle
    interpreter <- Interpreter.newHandle sources
    Repl.withHandle sources interpreter $ \repl ->
        Repl.run repl
