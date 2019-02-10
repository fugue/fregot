module Main where

import qualified Fregot.Repl as Repl

main :: IO ()
main = Repl.withHandle Repl.run
