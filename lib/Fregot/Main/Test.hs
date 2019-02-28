{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Main.Test
    ( Options
    , parseOptions

    , main
    ) where

import           Control.Lens            (view, (^.))
import           Control.Lens.TH         (makeLenses)
import           Control.Monad           (forM_)
import           Control.Monad.Parachute
import           Control.Monad.Trans     (liftIO)
import qualified Data.IORef              as IORef
import qualified Data.Text               as T
import qualified Fregot.Error            as Error
import qualified Fregot.Eval             as Eval
import qualified Fregot.Eval.Value       as Value
import qualified Fregot.Interpreter      as Interpreter
import qualified Fregot.PrettyPrint      as PP
import qualified Fregot.Sources          as Sources
import           Fregot.Sugar            (PackageName, Var)
import qualified Fregot.Sugar            as Sugar
import qualified Options.Applicative     as OA
import qualified System.IO               as IO

data Options = Options
    { _paths :: [FilePath]
    } deriving (Show)

$(makeLenses ''Options)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> (OA.some $ OA.strArgument $
            OA.metavar "PATHS" <>
            OA.help    "Rego files or directories to test")

runTest
    :: Interpreter.Handle -> PackageName -> Var -> Interpreter.InterpreterM ()
runTest h pkgname rule = do
    liftIO $ IO.hPutStrLn IO.stderr $
        "Running test " <> Sugar.packageNameToString pkgname <>
        "." <> Sugar.varToString rule <> "..."
    doc <- Interpreter.evalVar h pkgname rule

    forM_ doc $ \row -> liftIO $
        PP.hPutSemDoc IO.stderr $ "=" PP.<+> PP.pretty row
    liftIO $ IO.hPutStrLn IO.stderr $
        case doc of
            (_ : _) | all (isTrue . view Eval.rowValue) doc -> "OK"
            _                                               -> "FAIL"
  where
    isTrue (Value.BoolV b) = b
    isTrue _               = False

main :: Options -> IO ()
main opts = do
    sources <- Sources.newHandle
    interpreter <- Interpreter.newHandle sources
    (errors, _mbResult) <- runParachuteT $ do
        forM_ (opts ^. paths) $ \path -> Interpreter.loadModule interpreter path
        tests <- filter isTest <$> Interpreter.readRules interpreter
        forM_ tests $ \(pkg, rule) -> runTest interpreter pkg rule

    sources' <- IORef.readIORef sources
    Error.hPutErrors IO.stderr sources' Error.TextFmt errors
  where
    isTest (_pkgname, var) = "test_" `T.isPrefixOf` Sugar.varToText var
