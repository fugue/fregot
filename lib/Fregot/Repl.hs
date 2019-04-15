{-# LANGUAGE LambdaCase        #-}
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

import           Control.Lens                      (preview, review, (^.), (^?))
import           Control.Lens.TH                   (makeLenses)
import           Control.Monad.Extended            (foldMapM, forM_, guard,
                                                    void, when)
import           Control.Monad.Parachute
import           Control.Monad.Trans               (liftIO)
import           Data.Bifunctor                    (bimap)
import           Data.Char                         (isSpace)
import           Data.Functor                      (($>))
import qualified Data.HashMap.Strict.Extended      as HMS
import           Data.IORef.Extended               (IORef)
import qualified Data.IORef.Extended               as IORef
import qualified Data.List                         as L
import           Data.Maybe                        (fromMaybe, isNothing)
import qualified Data.Text                         as T
import           Data.Version                      (showVersion)
import qualified Fregot.Error                      as Error
import qualified Fregot.Eval                       as Eval
import qualified Fregot.Eval.Builtins              as Builtins
import qualified Fregot.Interpreter                as Interpreter
import qualified Fregot.Parser.Internal            as Parser
import qualified Fregot.Parser.Sugar               as Parser
import qualified Fregot.Prepare.Ast                as Prepare
import           Fregot.PrettyPrint                ((<$$>), (<+>))
import qualified Fregot.PrettyPrint                as PP
import qualified Fregot.Repl.Multiline             as Multiline
import           Fregot.Sources                    (SourcePointer)
import qualified Fregot.Sources                    as Sources
import           Fregot.Sources.SourceSpan         (SourceSpan)
import qualified Fregot.Sources.SourceSpan         as SourceSpan
import           Fregot.Sugar
import qualified Fregot.Test                       as Test
import           Fregot.Version                    (version)
import qualified System.Console.Haskeline.Extended as Hl
import qualified System.Directory                  as Directory
import           System.FilePath                   ((</>))
import qualified System.IO.Extended                as IO

data Mode
    = StartSteppingMode
    | SteppingMode (Eval.StepState Eval.Value)
    | RegularMode

data Handle = Handle
    { _sources     :: !Sources.Handle
    , _interpreter :: !Interpreter.Handle
    , _replCount   :: !(IORef Int)
    -- | Last file that was loaded.  Used to implement the `:reload` command.
    , _lastLoad    :: !(IORef (Maybe FilePath))
    -- | Currently open package.
    , _openPackage :: !(IORef PackageName)

    -- | Current mode; either debugging or regular evaluation.
    , _mode        :: !(IORef Mode)
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
    _replCount   <- IORef.newIORef 0
    _lastLoad    <- IORef.newIORef Nothing
    _openPackage <- IORef.newIORef "repl"
    _mode        <- IORef.newIORef RegularMode
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
                (Just idx, _) -> Just $ TermE a $
                    RefT a a
                        (r ^. ruleHead . ruleName) [RefBrackArg (TermE a idx)]
                (_, Just args) -> Just $ TermE a $
                    CallT a [r ^. ruleHead . ruleName] args
                _ -> Just $ TermE a $
                    VarT a (r ^. ruleHead . ruleName)

        | otherwise = Nothing
      where
        a = r ^. ruleHead . ruleAnn

processInput :: Handle -> T.Text -> IO ()
processInput h input | T.all isSpace input =
    -- NOTE(jaspervdj): Step if in stepping mode.
    processStep h
processInput h input = do
    (sourcep, mbRuleOrTerm) <- parseRuleOrExpr h input
    pkgname <- IORef.readIORef (h ^. openPackage)
    emode   <- IORef.readIORef (h ^. mode)
    case mbRuleOrTerm of
        Just (Left rule) -> do
            PP.hPutSemDoc IO.stdout $ PP.pretty rule
            void $ runInterpreter h $ \i -> do
                Interpreter.insertRule i pkgname sourcep rule
                Interpreter.compilePackages i

        Just (Right expr) | StartSteppingMode <- emode -> do
            mbStepState <- runInterpreter h $ \i ->
                Interpreter.mkStepState i pkgname expr
            forM_ mbStepState $ \stepState -> do
                IORef.writeIORef (h ^. mode) (SteppingMode stepState)
            processStep h

        Just (Right expr) -> do
            PP.hPutSemDoc IO.stdout $ PP.pretty expr
            let ctx = case emode of
                    SteppingMode state -> state ^. Eval.ssContext
                    _                  -> Eval.emptyContext
            mbRows <- runInterpreter h $ \i ->
                Interpreter.evalExpr i ctx pkgname expr
            forM_ mbRows $ \rows -> forM_ rows $ \row ->
                PP.hPutSemDoc IO.stdout $ "=" <+> PP.pretty row

        Nothing -> return ()

-- | Only makes sense in stepping mode.  Otherwise, does nothing.
processStep :: Handle -> IO ()
processStep h = do
    emode <- IORef.readIORef (h ^. mode)
    case emode of
        RegularMode        -> return ()
        StartSteppingMode  -> return ()
        SteppingMode state -> do
            mbStep <- runInterpreter h $ \i -> Interpreter.step i state
            case mbStep of
                Nothing                      ->
                    PP.hPutSemDoc IO.stdout $ prefix "internal error"
                Just Interpreter.Done        -> do
                    PP.hPutSemDoc IO.stdout $ prefix "finished"
                    IORef.writeIORef (h ^. mode) StartSteppingMode
                Just (Interpreter.Yield x nstate)   -> do
                    PP.hPutSemDoc IO.stdout $ prefix "=" <+> PP.pretty x
                    IORef.writeIORef (h ^. mode) (SteppingMode nstate)
                    processStep h
                Just (Interpreter.Suspend loc nstate) -> do
                    sauce <- IORef.readIORef (h ^. sources)
                    PP.hPutSemDoc IO.stdout $
                        case SourceSpan.citeSourceSpan PP.hint sauce loc of
                            Nothing -> "at" <+> PP.pretty loc
                            Just d  -> d
                    IORef.writeIORef (h ^. mode) (SteppingMode nstate)
                Just (Interpreter.Error e)   -> do
                    PP.hPutSemDoc IO.stdout $ prefix "error"
                    sauce <- IORef.readIORef (h ^. sources)
                    Error.hPutErrors IO.stderr sauce Error.TextFmt [e]
  where
    prefix = (PP.hint "(debug)" <+>)

run :: Handle -> IO ()
run h = do
    IO.hPutStrLn IO.stderr $ L.intersperse ' ' "Fugue REGO Toolkit"
    IO.hPutStrLn IO.stderr $
        "fregot v" <> showVersion version <> " repl - use :help for usage info"

    home <- Directory.getHomeDirectory
    let settings = Hl.Settings
            { Hl.historyFile    = Just (home </> ".fregot.repl")
            , Hl.autoAddHistory = True
            , Hl.complete       = Hl.concatCompletion
                [ Hl.completeFilename
                , completeBuiltins h
                , completeRules h
                , completePackages h
                , completePackageRules h
                ]
            }

    Hl.runInputT settings loop
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
        prompt  <- liftIO getPrompt
        mbLine0 <- Hl.getInputLine prompt
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

    getPrompt :: IO String
    getPrompt = do
        pkg   <- IORef.readIORef (h ^. openPackage)
        emode <- IORef.readIORef (h ^. mode)
        return $
            review packageNameFromString pkg <>
            (case emode of
                RegularMode       -> ""
                SteppingMode _    -> "(debug)"
                StartSteppingMode -> "(debug)") <>
            "% "

metaShortcuts :: HMS.HashMap T.Text MetaCommand
metaShortcuts =
    let base = HMS.fromList $ map (\m -> (m ^. metaName, m)) metaCommands in
    HMS.shortcuts shortcuts base
  where
    shortcuts = HMS.fromList
        [ (":h", ":help")
        , (":l", ":load")
        , (":o", ":open")
        , (":q", ":quit")
        , (":r", ":reload")
        , (":t", ":test")
        ]

metaCommands :: [MetaCommand]
metaCommands =
    [ MetaCommand ":debug" "Toggle debug mode" $ \h _ -> do
        liftIO $ IORef.atomicModifyIORef_ (h ^. mode) $ \case
            RegularMode       -> StartSteppingMode
            StartSteppingMode -> RegularMode
            SteppingMode _    -> RegularMode
        return True
    , MetaCommand ":help" "show this info" $ \_ _ -> do
        liftIO $ PP.hPutSemDoc IO.stderr $
            "Enter an expression to evaluate it." <$$>
            "Enter a rule to add it to the current package." <$$>
            mempty <$$>
            "Other commands:" <$$>
            (PP.ind $ PP.vcat $ do
                MetaCommand {..} <- metaCommands
                return $ PP.pretty _metaName <> "  " <>
                    PP.pretty _metaDescription) <$$>
            mempty <$$>
            "Shortcuts are supported for commands, e.g. `:l` for `:load`."
        return True
    , MetaCommand ":load" "load a rego file, e.g. `:load foo.rego`" $
        \h args -> case args of
            _ | [path] <- T.unpack <$> args -> liftIO $ load h path
            _ -> do
                liftIO $ IO.hPutStrLn IO.stderr $
                    ":load takes one path argument"
                return True
    , MetaCommand ":open" "open a different package, e.g. `:open foo`" $
        \h args -> case args of
            _ | [Just pkg] <- preview packageNameFromText <$> args ->
                -- TODO(jaspervdj): Check if exists?
                liftIO $ IORef.writeIORef (h ^. openPackage) pkg $> True
            _ -> do
                liftIO $ IO.hPutStrLn IO.stderr $
                    ":open takes a package name as argument"
                return True
    , MetaCommand ":quit" "exit the repl" $ \_ _ -> return False
    , MetaCommand ":reload" "reload the file from the last `:load`" $
        \h _ -> liftIO $ do
            mbLastLoad <- IORef.readIORef (h ^. lastLoad)
            case mbLastLoad of
                Just ll -> load h ll
                Nothing -> IO.hPutStrLn IO.stderr "No files loaded" $> True
    , MetaCommand ":test" "run tests in the current package" $
        \h _ -> liftIO $ do
            pkg     <- IORef.readIORef (h ^. openPackage)
            results <- runInterpreter h $ \i -> do
                rules <- map ((,) pkg) <$> Interpreter.readPackageRules i pkg
                foldMapM (Test.runTest i) $ filter Test.isTest rules
            sauce <- IORef.readIORef (h ^. sources)
            forM_ results (Test.printTestResults IO.stdout sauce)
            return True
    ]
  where
    load h path = do
        IO.hPutStrLn IO.stderr $ "Loading " ++ path ++ "..."
        IORef.writeIORef (h ^. lastLoad) (Just path)
        void $ runInterpreter h $ \i -> do
            Interpreter.loadModule i path
            Interpreter.compilePackages i
        return True

completeBuiltins :: Handle -> Hl.CompletionFunc IO
completeBuiltins _h = Hl.completeDictionary completeWhitespace $ return
    [ nestedVarToString (NestedVar vs)
    | (Prepare.NamedFunction vs, _) <- HMS.toList Builtins.builtins
    ]

completeRules :: Handle -> Hl.CompletionFunc IO
completeRules h = Hl.completeDictionary completeWhitespace $ do
    pkg     <- IORef.readIORef (h ^. openPackage)
    results <- runInterpreter h $ \i -> Interpreter.readPackageRules i pkg
    return $ map varToString $ fromMaybe [] results

completePackages :: Handle -> Hl.CompletionFunc IO
completePackages h = Hl.completeDictionary completeWhitespace $ do
    pkgs <- fromMaybe [] <$> runInterpreter h Interpreter.readPackages
    return (map ((<> ".") . review dataPackageNameFromString) pkgs)

completePackageRules :: Handle -> Hl.CompletionFunc IO
completePackageRules h = Hl.completeWord Nothing completeWhitespace $ \str0 -> do
    let (prefix, pkgname) =
            bimap reverse (reverse . drop 1) $
            break (== '.') (reverse str0)
    case pkgname ^? dataPackageNameFromString of
        Nothing      -> return []
        Just pkg -> do
            rules <- runInterpreter h $ \i -> Interpreter.readPackageRules i pkg
            return $ do
                rule <- fromMaybe [] rules
                let r = varToString rule
                    text = review dataPackageNameFromString pkg <> "." <> r
                guard $ prefix `L.isPrefixOf` r
                return (Hl.Completion text text False)

completeWhitespace :: String
completeWhitespace = "(){}=;:+-/* \t\n"
