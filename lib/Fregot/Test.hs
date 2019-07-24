{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Test
    ( TestName
    , TestResults (..), passed, failed, errored

    , isTest
    , runTest
    , printTestResults
    ) where

import           Control.Lens              (review, view, (&), (.~), (^.))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad.Extended    (forM_, unless)
import qualified Control.Monad.Parachute   as Parachute
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
import qualified System.IO                 as IO

type TestName = (PackageName, Var)

isTest :: TestName -> Bool
isTest (_pkgname, var) = "test_" `T.isPrefixOf` Sugar.varToText var

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
            mempty & errored .~ [(testname, errs)]

    return results
  where
    isTrue (Value.BoolV b) = b
    isTrue _               = False

    source = SourceSpan.testSourceSpan

printTestResults
    :: IO.Handle -> Sources.Sources -> TestResults -> IO ()
printTestResults h sources tr = do
    IO.hPutStrLn h $
        "passed: " ++ show (length (tr ^. passed)) ++
        ", failed: " ++ show (length (tr ^. failed)) ++
        ", errored: " ++ show (length (tr ^. errored))

    forM_ (tr ^. errored) $ \((pkg, rule), terrs) -> do
        IO.hPutStrLn h $ review Sugar.packageNameFromString pkg <>
            "." <> Sugar.varToString rule <> ":"
        Error.hPutErrors h sources Error.Text terrs

    unless (null (tr ^. failed)) $ do
        IO.hPutStrLn h "Failed tests:"
        forM_ (tr ^. failed) $ \(pkg, rule) -> do
            IO.hPutStrLn h $
                "- " <> review Sugar.packageNameFromString pkg <> "." <>
                Sugar.varToString rule
