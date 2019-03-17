{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Repl
    ( Handle
    , withHandle
    , run

    , MetaCommand (..), metaName, metaDescription, metaRun
    , metaCommands
    ) where

import           Control.Lens                 ((^.))
import           Control.Lens.TH              (makeLenses)
import           Control.Monad                (forM_, void, when)
import           Control.Monad.Parachute
import           Control.Monad.Trans          (liftIO)
import           Data.Char                    (isSpace)
import qualified Data.HashMap.Strict.Extended as HMS
import           Data.IORef.Extended          (IORef)
import qualified Data.IORef.Extended          as IORef
import qualified Data.List                    as L
import           Data.Maybe                   (isNothing)
import qualified Data.Text                    as T
import           Data.Version                 (showVersion)
import qualified Fregot.Error                 as Error
import qualified Fregot.Interpreter           as Interpreter
import qualified Fregot.Parser.Internal       as Parser
import qualified Fregot.Parser.Sugar          as Parser
import           Fregot.PrettyPrint           ((<$$>))
import qualified Fregot.PrettyPrint           as PP
import qualified Fregot.Repl.Multiline        as Multiline
import           Fregot.Sources               (SourcePointer)
import qualified Fregot.Sources               as Sources
import           Fregot.Sources.SourceSpan    (SourceSpan)
import           Fregot.Sugar
import           Fregot.Version               (version)
import qualified System.Console.Haskeline     as Hl
import qualified System.IO.Extended           as IO

data Handle = Handle
    { _sources     :: !Sources.Handle
    , _interpreter :: !Interpreter.Handle
    , _replCount   :: !(IORef Int)
    }

data MetaCommand = MetaCommand
    { _metaName        :: !T.Text
    , _metaDescription :: !T.Text
    , _metaRun         :: Handle -> [T.Text] -> Hl.InputT IO Bool
    }

$(makeLenses ''Handle)
$(makeLenses ''MetaCommand)

withHandle
    :: Sources.Handle
    -> Interpreter.Handle
    -> (Handle -> IO a)
    -> IO a
withHandle _sources _interpreter f = do
    _replCount <- IORef.newIORef 0
    f Handle {..}

-- | Auxiliary function to invoke the interpreter.
runInterpreter
    :: Handle -> (Interpreter.Handle -> Interpreter.InterpreterM a)
    -> IO (Maybe a)
runInterpreter h f = do
    (errors, mbX) <- runParachuteT $ f (h ^. interpreter)
    sauce <- IORef.readIORef (h ^. sources)
    Error.hPutErrors IO.stderr sauce Error.TextFmt errors
    return mbX

parseRuleOrExpr
    :: Handle -> T.Text
    -> IO (SourcePointer, Maybe (Either (Rule SourceSpan) (Expr SourceSpan)))
parseRuleOrExpr h input = do
    replNum <- IORef.atomicModifyIORef (h ^. replCount) $ \x -> (x + 1, x)
    let sourcep = Sources.ReplInput replNum input
    IORef.atomicModifyIORef_ (h ^. sources) $ Sources.insert sourcep input

    (ruleErrors, mbRule) <- runParachuteT $
        Parser.lexAndParse Parser.rule sourcep input
    sauce <- IORef.readIORef (h ^. sources)
    case mbRule of
        Just r | Just e <- ruleToExpr r -> do
            Error.hPutErrors IO.stderr sauce Error.TextFmt ruleErrors
            return (sourcep, Just $ Right e)

        Just r -> do
            Error.hPutErrors IO.stderr sauce Error.TextFmt ruleErrors
            return (sourcep, Just $ Left r)

        Nothing -> do
            (exprErrors, mbExpr) <- runParachuteT $
                Parser.lexAndParse Parser.expr sourcep input
            Error.hPutErrors IO.stderr sauce Error.TextFmt exprErrors
            return (sourcep, fmap Right mbExpr)
  where
    ruleToExpr r
        | null (r ^. ruleBodies)
        , isNothing (r ^. ruleHead . ruleValue) =
            case (r ^. ruleHead . ruleIndex, r ^. ruleHead . ruleArgs) of
                -- TODO (jaspervdj): undefined
                (Just idx, _) -> Just $ TermE undefined $
                    RefT undefined undefined
                        (r ^. ruleHead . ruleName) [RefBrackArg (TermE undefined idx)]
                (_, Just args) -> Just $ TermE undefined $
                    CallT undefined [r ^. ruleHead . ruleName] args
                _ -> Just $ TermE undefined $
                    VarT undefined (r ^. ruleHead . ruleName)

        | otherwise = Nothing

processInput :: Handle -> T.Text -> IO ()
processInput _h input | T.all isSpace input = return ()
processInput h input = do
    (sourcep, mbRuleOrTerm) <- parseRuleOrExpr h input
    case mbRuleOrTerm of
        Just (Left rule) -> do
            PP.hPutSemDoc IO.stdout $ PP.pretty rule
            void $ runInterpreter h $ \i ->
                Interpreter.insertRule i "repl" sourcep rule

        Just (Right expr) -> do
            PP.hPutSemDoc IO.stdout $ PP.pretty expr
            mbRows <- runInterpreter h $ \i ->
                Interpreter.evalExpr i "repl" expr
            forM_ mbRows $ \rows -> forM_ rows $ \row ->
                PP.hPutSemDoc IO.stdout $ "=" PP.<+> PP.pretty row
        Nothing -> return ()

run :: Handle -> IO ()
run h = do
    IO.hPutStrLn IO.stderr $ L.intersperse ' ' "Fugue REGO Toolkit"
    IO.hPutStrLn IO.stderr $
        "fregot v" <> showVersion version <> " repl - use :help for usage info"
    Hl.runInputT Hl.defaultSettings loop
  where
    loop :: Hl.InputT IO ()
    loop = do
        mbInput <- getMultilineInput
        case mbInput of
            Nothing -> return ()
            Just input
                    | (meta : args) <- T.words input
                    , ":" `T.isPrefixOf` meta
                    , Just cmd <- HMS.lookup meta metaShortcuts -> do
                cont <- (cmd ^. metaRun) h args
                when cont loop
            Just input   -> do
                liftIO $ processInput h input
                loop

    getMultilineInput :: Hl.InputT IO (Maybe T.Text)
    getMultilineInput = do
        mbLine0 <- Hl.getInputLine "% "
        case mbLine0 of
            Nothing    -> return Nothing
            Just input -> more Multiline.emptyPartial input
       where
        more p0 line = case Multiline.feed p0 (T.pack line) of
            Multiline.Complete txt -> return $ Just txt
            Multiline.Partial  p1  -> do
                mbNextLine <- Hl.getInputLine "  "
                case mbNextLine of
                    Nothing       -> return $ Just $ Multiline.finish p1
                    Just nextLine -> more p1 nextLine

metaShortcuts :: HMS.HashMap T.Text MetaCommand
metaShortcuts =
    let base = HMS.fromList $ map (\m -> (m ^. metaName, m)) metaCommands in
    HMS.shortcuts shortcuts base
  where
    shortcuts = HMS.fromList
        [ (":h", ":help")
        , (":l", ":load")
        , (":q", ":quit")
        ]

metaCommands :: [MetaCommand]
metaCommands =
    [ MetaCommand ":help" "show this info" $ \_ _ -> do
        liftIO $ PP.hPutSemDoc IO.stderr $
            "Enter an expression to evaluate it." <$$>
            "Enter a rule to add it to the current package." <$$>
            mempty <$$>
            "Other commands:" <$$>
            (PP.ind $ PP.vcat $ do
                MetaCommand {..} <- metaCommands
                return $ PP.pretty _metaName <> "  " <>
                    PP.pretty _metaDescription)
        return True
    , MetaCommand ":quit" "exit the repl" $ \_ _ -> return False
    , MetaCommand ":load" "load a rego file, e.g. `:load foo.rego`" $
        \h args -> case args of
            _ | [path] <- T.unpack <$> args -> liftIO $ do
                IO.hPutStrLn IO.stderr $ "Loading " ++ path ++ "..."
                void $ runInterpreter h $ \i -> do
                    Interpreter.loadModule i path
                    Interpreter.compilePackages i
                return True
            _ -> do
                liftIO $ IO.hPutStrLn IO.stderr $
                    ":load takes one path argument"
                return True
    ]
