module Fregot.Test
    ( main
    ) where

import           Control.Monad           (forM_)
import           Control.Monad.Parachute
import           Control.Monad.Trans     (liftIO)
import qualified Data.IORef              as IORef
import qualified Fregot.Error            as Error
import qualified Fregot.Interpreter      as Interpreter
import qualified Fregot.Sources          as Sources
import           System.Environment      (getArgs)
import qualified System.IO               as IO

main :: IO ()
main = do
    sources <- Sources.newHandle
    (errors, _mbResult) <- runParachuteT $
        Interpreter.withHandle sources $ \interpreter -> do
            args <- liftIO getArgs
            forM_ args $ \arg ->
                Interpreter.loadPackageFile interpreter arg

    sources' <- IORef.readIORef sources
    Error.hPutErrors IO.stderr sources' Error.TextFmt errors
