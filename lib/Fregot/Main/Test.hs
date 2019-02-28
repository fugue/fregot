{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Main.Test
    ( Options
    , parseOptions

    , main
    ) where

import           Control.Lens            (view, (&), (.~), (^.))
import           Control.Lens.TH         (makeLenses)
import           Control.Monad.Extended  (foldMapM, forM_)
import           Control.Monad.Parachute
import qualified Data.IORef              as IORef
import qualified Data.Text               as T
import qualified Fregot.Error            as Error
import qualified Fregot.Eval             as Eval
import qualified Fregot.Eval.Value       as Value
import qualified Fregot.Interpreter      as Interpreter
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

type TestName = (PackageName, Var)

data TestResults = TestResults
    { _passed :: [TestName]
    , _failed :: [TestName]
    -- TODO(jaspervdj): add errors
    }

$(makeLenses ''TestResults)

instance Semigroup TestResults where
    TestResults p1 f1 <> TestResults p2 f2 = TestResults (p1 <> p2) (f1 <> f2)

instance Monoid TestResults where
    mempty = TestResults [] []

runTest
    :: Interpreter.Handle -> PackageName -> Var
    -> Interpreter.InterpreterM TestResults
runTest h pkgname rule = do
    doc <- Interpreter.evalVar h pkgname rule

    let pass = case doc of
            (_ : _) | all (isTrue . view Eval.rowValue) doc -> True
            _                                               -> False

    return $ mempty & (if pass then passed else failed) .~ [(pkgname, rule)]
  where
    isTrue (Value.BoolV b) = b
    isTrue _               = False

main :: Options -> IO ()
main opts = do
    sources <- Sources.newHandle
    interpreter <- Interpreter.newHandle sources
    (errors, mbResult) <- runParachuteT $ do
        forM_ (opts ^. paths) $ \path -> Interpreter.loadModule interpreter path
        tests <- filter isTest <$> Interpreter.readRules interpreter
        foldMapM (\(pkg, rule) -> runTest interpreter pkg rule) tests

    forM_ mbResult $ \tr ->
        putStrLn $
            "passed: " ++ show (length (tr ^. passed)) ++
            ", failed: " ++ show (length (tr ^. failed))

    sources' <- IORef.readIORef sources
    Error.hPutErrors IO.stderr sources' Error.TextFmt errors
  where
    isTest (_pkgname, var) = "test_" `T.isPrefixOf` Sugar.varToText var
