module Fregot.Test
    ( main
    ) where

import           Control.Lens            (view)
import           Control.Monad           (forM_)
import           Control.Monad.Parachute
import           Control.Monad.Trans     (liftIO)
import qualified Data.IORef              as IORef
import qualified Fregot.Error            as Error
import qualified Fregot.Eval             as Eval
import qualified Fregot.Eval.Value       as Value
import qualified Fregot.Interpreter      as Interpreter
import qualified Fregot.Sources          as Sources
import           Fregot.Sugar            (PackageName, Var)
import qualified Fregot.Sugar            as Sugar
import           System.Environment      (getArgs)
import qualified System.IO               as IO

runTest
    :: Interpreter.Handle -> PackageName -> Var -> Interpreter.InterpreterM ()
runTest h pkgname rule = do
    liftIO $ IO.hPutStrLn IO.stderr $
        "Running test " <> Sugar.packageNameToString pkgname <>
        "." <> Sugar.varToString rule <> "..."
    doc <- Interpreter.evalVar h pkgname rule
    liftIO $ IO.hPutStrLn IO.stderr $
        if all (isTrue . view Eval.rowValue) doc then "OK" else "FAIL"
  where
    isTrue (Value.BoolV b) = b
    isTrue _               = False

main :: IO ()
main = do
    sources <- Sources.newHandle
    interpreter <- Interpreter.newHandle sources
    (errors, _mbResult) <- runParachuteT $ do
        args <- liftIO getArgs
        forM_ args $ \arg -> Interpreter.loadModule interpreter arg
        rules <- Interpreter.readRules interpreter
        forM_ rules $ \(pkg, rule) -> runTest interpreter pkg rule

    sources' <- IORef.readIORef sources
    Error.hPutErrors IO.stderr sources' Error.TextFmt errors
