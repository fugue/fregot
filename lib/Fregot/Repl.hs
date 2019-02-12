{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Repl
    ( Handle
    , withHandle
    , run
    ) where

import           Control.Lens              ((^.))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad             (forM_)
import           Control.Monad.Parachute
import           Control.Monad.Trans       (liftIO)
import           Data.Char                 (isSpace)
import           Data.IORef                (IORef)
import qualified Data.IORef                as IORef
import qualified Data.List                 as L
import           Data.Maybe                (isNothing)
import qualified Data.Text                 as T
import qualified Fregot.Error              as Error
import qualified Fregot.Eval               as Eval
import qualified Fregot.Eval.Rules         as Eval.Rules
import qualified Fregot.Parser.Internal    as Parser
import qualified Fregot.Parser.Sugar       as Parser
import qualified Fregot.PrettyPrint        as PP
import qualified Fregot.Repl.Multiline     as Multiline
import qualified Fregot.Sources            as Sources
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Fregot.Sugar
import           Prelude                   hiding (head)
import qualified System.Console.Haskeline  as Hl
import qualified System.IO.Extended        as IO

data Handle = Handle
    { _rules :: !(IORef Eval.Rules)
    }

$(makeLenses ''Handle)

withHandle :: (Handle -> IO a) -> IO a
withHandle f = do
    _rules <- IORef.newIORef Eval.Rules.empty
    f Handle {..}

parseRuleOrExpr
    :: Handle -> T.Text
    -> IO (Maybe (Either (Rule SourceSpan) (Expr SourceSpan)))
parseRuleOrExpr _h input = do
    (ruleErrors, mbRule) <- runParachuteT $
        Parser.lexAndParse Parser.rule sourcep input
    Error.hPutErrors IO.stderr sources Error.TextFmt ruleErrors
    case mbRule of
        Just r | Just e <- ruleToExpr r -> do
            Error.hPutErrors IO.stderr sources Error.TextFmt ruleErrors
            return $ Just $ Right e

        Just r -> do
            Error.hPutErrors IO.stderr sources Error.TextFmt ruleErrors
            return $ Just $ Left r

        Nothing -> do
            (exprErrors, mbExpr) <- runParachuteT $
                Parser.lexAndParse Parser.expr sourcep input
            Error.hPutErrors IO.stderr sources Error.TextFmt exprErrors
            return $ fmap Right mbExpr
  where
    sourcep = Sources.ReplInput 0 input
    sources = Sources.insert sourcep input Sources.empty

    ruleToExpr r
        | null (r ^. body)
        , isNothing (r ^. head . value) = case r ^. head . index of
            Nothing -> Just $ TermE undefined $
                VarT undefined (r ^. head . name)
            Just idx -> Just $ TermE undefined $
                RefT undefined undefined (r ^. head . name) [RefBrackArg idx]

        | otherwise = Nothing

processInput :: Handle -> T.Text -> IO ()
processInput _h input | T.all isSpace input = return ()
processInput h input = do
    mbRuleOrTerm <- parseRuleOrExpr h input
    case mbRuleOrTerm of
        Just (Left rule) -> do
            PP.hPutSemDoc IO.stdout $ PP.pretty rule
            IORef.modifyIORef' (h ^. rules) $ \rs -> Eval.Rules.insert rule rs
        Just (Right expr) -> do
            PP.hPutSemDoc IO.stdout $ PP.pretty expr
            rules0 <- IORef.readIORef (h ^. rules)
            let rows = Eval.runEvalM rules0 (Eval.evalExpr expr)
            forM_ rows $ \row -> PP.hPutSemDoc IO.stdout $
                "=" PP.<+> PP.pretty row
        Nothing -> return ()

run :: Handle -> IO ()
run h = do
    IO.hPutStrLn IO.stderr $ L.intersperse ' ' "Fugue REGO Toolkit"
    Hl.runInputT Hl.defaultSettings loop
  where
    loop :: Hl.InputT IO ()
    loop = do
        mbInput <- getMultilineInput
        case mbInput of
            Nothing     -> return ()
            Just "quit" -> return ()
            Just input  -> do
                liftIO $ processInput h input
                loop

    getMultilineInput :: Hl.InputT IO (Maybe T.Text)
    getMultilineInput = do
        mbLine0 <- Hl.getInputLine "% "
        case mbLine0 of
            Nothing     -> return Nothing
            Just input  -> more Multiline.emptyPartial input
       where
        more p0 line = case Multiline.feed p0 (T.pack line) of
            Multiline.Complete txt -> return $ Just txt
            Multiline.Partial  p1  -> do
                mbNextLine <- Hl.getInputLine "  "
                case mbNextLine of
                    Nothing       -> return $ Just $ Multiline.finish p1
                    Just nextLine -> more p1 nextLine
