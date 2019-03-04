{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Main.Test
    ( Options
    , parseOptions

    , main
    ) where

import           Control.Lens              (view, (&), (.~), (^.))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad.Extended    (foldMapM, forM_, unless)
import qualified Control.Monad.Parachute   as Parachute
import qualified Data.IORef                as IORef
import qualified Data.Text                 as T
import           Fregot.Error              (Error)
import qualified Fregot.Error              as Error
import qualified Fregot.Eval               as Eval
import qualified Fregot.Eval.Value         as Value
import qualified Fregot.Interpreter        as Interpreter
import qualified Fregot.Sources            as Sources
import qualified Fregot.Sources.SourceSpan as SourceSpan
import           Fregot.Sugar              (PackageName, Var)
import qualified Fregot.Sugar              as Sugar
import qualified Options.Applicative       as OA
import           System.Exit               (ExitCode (..))
import qualified System.IO                 as IO

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
    { _passed  :: [TestName]
    , _failed  :: [TestName]
    , _errored :: [(TestName, [Error])]
    }

$(makeLenses ''TestResults)

instance Semigroup TestResults where
    TestResults p1 f1 e1 <> TestResults p2 f2 e2 =
        TestResults (p1 <> p2) (f1 <> f2) (e1 <> e2)

instance Monoid TestResults where
    mempty = TestResults [] [] []

runTest
    :: Interpreter.Handle -> TestName
    -> Interpreter.InterpreterM TestResults
runTest h testname@(pkgname, rule) = do
    errOrDoc <- Parachute.try $ Interpreter.evalVar h source pkgname rule

    results <- case errOrDoc of
        Right doc@(_ : _) | all (isTrue . view Eval.rowValue) doc -> return $
            mempty & passed .~ [testname]
        Right _ -> return $
            mempty & failed .~ [testname]
        Left errs -> return $
            -- TODO(jaspervdj): Only print errors when running in verbose?
            mempty & errored .~ [(testname, errs)]

    return results
  where
    isTrue (Value.BoolV b) = b
    isTrue _               = False

    source = SourceSpan.testSourceSpan

main :: Options -> IO ExitCode
main opts = do
    sources <- Sources.newHandle
    interpreter <- Interpreter.newHandle sources
    (errors, mbResult) <- Parachute.runParachuteT $ do
        forM_ (opts ^. paths) $ \path -> Interpreter.loadModule interpreter path
        tests <- filter isTest <$> Interpreter.readRules interpreter
        foldMapM (\t -> runTest interpreter t) tests

    sources' <- IORef.readIORef sources
    forM_ mbResult $ \tr -> do
        putStrLn $
            "passed: " ++ show (length (tr ^. passed)) ++
            ", failed: " ++ show (length (tr ^. failed)) ++
            ", errored: " ++ show (length (tr ^. errored))

        forM_ (tr ^. errored) $ \((pkg, rule), terrs) -> do
            IO.hPutStrLn IO.stderr $ Sugar.packageNameToString pkg <> "." <>
                Sugar.varToString rule <> ":"
            Error.hPutErrors IO.stderr sources' Error.TextFmt terrs

        unless (null (tr ^. failed)) $ do
            IO.hPutStrLn IO.stderr "Failed tests:"
            forM_ (tr ^. failed) $ \(pkg, rule) -> do
                IO.hPutStrLn IO.stderr $
                    "- " <> Sugar.packageNameToString pkg <> "." <>
                    Sugar.varToString rule

    Error.hPutErrors IO.stderr sources' Error.TextFmt errors

    return $! case mbResult of
        Just tr | null (tr ^. failed) && null (tr ^. errored) -> ExitSuccess
        _ -> ExitFailure 1
  where
    isTest (_pkgname, var) = "test_" `T.isPrefixOf` Sugar.varToText var
