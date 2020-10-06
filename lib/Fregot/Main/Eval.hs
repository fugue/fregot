{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Main.Eval
    ( Options
    , parseOptions

    , main
    ) where

import           Control.Lens                 (view, (^.))
import           Control.Lens.TH              (makeLenses)
import           Control.Monad.Extended       (forM_)
import qualified Control.Monad.Parachute      as Parachute
import qualified Data.Aeson                   as A
import qualified Data.ByteString.Lazy         as BL
import           Data.Foldable                (traverse_)
import qualified Data.IORef                   as IORef
import qualified Data.Text                    as T
import qualified Fregot.Error                 as Error
import           Fregot.Eval                  (rowValue)
import           Fregot.Eval.Json             as Json
import qualified Fregot.Find                  as Find
import qualified Fregot.Interpreter           as Interpreter
import           Fregot.Main.GlobalOptions
import           Fregot.Names
import qualified Fregot.Parser                as Parser
import qualified Fregot.PrettyPrint           as PP
import           Fregot.Repl.Parse
import qualified Fregot.Sources               as Sources
import           Fregot.Sugar                 (ruleAnn, ruleHead)
import qualified Options.Applicative.Extended as OA
import           System.Exit                  (ExitCode (..))
import qualified System.IO                    as IO

data Options = Options
    { _input      :: !(Maybe FilePath)
    , _expression :: !T.Text
    , _paths      :: [DestinationPrefix FilePath]
    } deriving (Show)

$(makeLenses ''Options)

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> inputPath
    <*> (fmap T.pack $ OA.strArgument $
            OA.metavar "EXPRESSION" <>
            OA.help    "Rego expression to evaluate")
    <*> (OA.many $ fmap parseDestinationPrefix $ OA.strArgument $
            OA.metavar "PATHS" <>
            OA.help    "Rego files or directories to load")

main :: GlobalOptions -> Options -> IO ExitCode
main gopts opts = do
    sources <- Sources.newHandle
    interpreter <- Interpreter.newHandle
        (Interpreter.defaultConfig {Interpreter._dumpTags = gopts ^. dumpTags})
        sources
    regoPaths <- Find.findPrefixedRegoFiles (opts ^. paths)
    (errors, mbResult) <- Parachute.runParachuteT $ do
        forM_ regoPaths $ Interpreter.loadFileByExtension
            interpreter Parser.defaultParserOptions
        Interpreter.compileRules interpreter

        traverse_ (Interpreter.setInputFile interpreter) (opts ^. input)

        ruleOrExpr <- parseRuleOrQuery Sources.CliInput (opts ^. expression)
        expr <- case ruleOrExpr of
            Right e -> return e
            Left r  -> Parachute.fatal $ Error.mkError
                "eval" (r ^. ruleHead . ruleAnn) "unexpected rule"
                "Need an expression to evaluate, not a rule"

        val <- Interpreter.evalQuery interpreter Nothing "cli" expr
        case traverse (Json.fromValue . view rowValue) val of
            Right js -> return $ A.toJSON js
            Left  e  -> Parachute.fatal $ Error.mkErrorNoMeta
                "eval" $ "invalid json:" PP.<+> e

    sources' <- IORef.readIORef sources
    Error.hPutErrors IO.stderr sources' (gopts ^. format) errors

    case mbResult of
        Nothing -> return (ExitFailure 1)
        Just j  -> do
            BL.hPutStr IO.stdout $! A.encode j
            IO.hPutStrLn IO.stdout ""
            return ExitSuccess
