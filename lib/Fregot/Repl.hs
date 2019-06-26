{-# LANGUAGE DeriveGeneric     #-}
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

import           Control.Lens                      (preview, review, view, (&),
                                                    (.~), (^.), (^?))
import           Control.Lens.TH                   (makeLenses)
import           Control.Monad                     (unless)
import           Control.Monad.Extended            (foldMapM, forM_, guard,
                                                    void, when)
import           Control.Monad.Parachute
import           Control.Monad.Trans               (liftIO)
import           Data.Bifunctor                    (bimap)
import           Data.Functor                      (($>))
import qualified Data.HashMap.Strict.Extended      as HMS
import qualified Data.HashSet                      as HS
import           Data.IORef.Extended               (IORef)
import qualified Data.IORef.Extended               as IORef
import qualified Data.List                         as L
import           Data.Maybe                        (fromMaybe, isNothing)
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T
import           Data.Version                      (showVersion)
import qualified Fregot.Error                      as Error
import qualified Fregot.Error.Stack                as Stack
import qualified Fregot.Eval                       as Eval
import qualified Fregot.Eval.Builtins              as Builtins
import qualified Fregot.Eval.Value                 as Eval
import qualified Fregot.Interpreter                as Interpreter
import           Fregot.Names
import qualified Fregot.Prepare.Ast                as Prepare
import           Fregot.PrettyPrint                ((<$$>), (<+>))
import qualified Fregot.PrettyPrint                as PP
import           Fregot.Repl.Breakpoint
import qualified Fregot.Repl.Multiline             as Multiline
import           Fregot.Repl.Parse
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
    = RegularMode
    | Suspended   SourceSpan (Eval.StepState Eval.Value)
    | Errored     Eval.Environment Eval.Context Error.Error

data StepTo
    = StepToBreak (Maybe Stack.StackTrace)
    | StepInto
    | StepOver    Stack.StackTrace

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
    , _breakpoints :: !(IORef (HS.HashSet Breakpoint))
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
    _breakpoints <- IORef.newIORef HS.empty
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

-- | Retrieve the "currently focused package".  This is usually the
-- `_openPackage` IORef, but if we are currently debugging/paused, we use that
-- package.
readFocusedPackage
    :: Handle -> IO PackageName
readFocusedPackage h = do
    open  <- IORef.readIORef (h ^. openPackage)
    emode <- IORef.readIORef (h ^. mode)
    let stack = case emode of
            RegularMode    -> Nothing
            Suspended _ ss -> Just $ ss ^. Eval.ssEnvironment . Eval.stack
            Errored e _ _  -> Just $ e ^. Eval.stack
    return $ fromMaybe open (stack >>= Stack.package)

processInput :: Handle -> T.Text -> IO ()
processInput h input = do
    replNum <- IORef.atomicModifyIORef (h ^. replCount) $ \x -> (x + 1, x)
    let sourcep = Sources.ReplInput replNum input
    IORef.atomicModifyIORef_ (h ^. sources) $ Sources.insert sourcep input

    (parseErrs, mbRuleOrTerm) <- runParachuteT $ parseRuleOrExpr sourcep input
    sauce <- IORef.readIORef (h ^. sources)
    Error.hPutErrors IO.stderr sauce Error.TextFmt parseErrs

    emode   <- IORef.readIORef (h ^. mode)
    bkpts   <- IORef.readIORef (h ^. breakpoints)
    pkgname <- readFocusedPackage h
    case mbRuleOrTerm of
        Just (Left rule) | RegularMode <- emode -> do
            mbResult <- runInterpreter h $ \i -> do
                Interpreter.insertRule i pkgname sourcep rule
                Interpreter.compilePackages i
            unless (isNothing mbResult) $ PP.hPutSemDoc IO.stderr $
                "Rule" <+>
                PP.code (PP.pretty (rule ^. ruleHead . ruleName)) <+>
                "added"

        Just (Left _rule) ->
            -- NOTE(jaspervdj): I think it shouldn't be /too/ hard to allow
            -- this, but I'm not sure if it's worth it.
            IO.hPutStrLn IO.stderr $ "Cannot add rules while debugging"

        Just (Right expr) | not (HS.null bkpts), RegularMode <- emode -> do
            mbStepState <- runInterpreter h $ \i ->
                Interpreter.mkStepState i pkgname expr

            case mbStepState of
                Nothing     -> return ()
                Just sstate -> processStep h (StepToBreak Nothing) sstate

        Just (Right expr) -> do
            mbRows <- runInterpreter h $ \i -> do
                -- Read and patch the options.
                eopts <- Interpreter.readEvalOptions i
                let eopts' = case emode of
                        Suspended _ state -> eopts
                            & Interpreter.eoContext  .~ (state ^. Eval.ssContext)
                            & Interpreter.eoInputDoc .~ (state ^. Eval.ssEnvironment . Eval.inputDoc)
                        Errored env ctx _  -> eopts
                            & Interpreter.eoContext  .~ ctx
                            & Interpreter.eoInputDoc .~ (env ^. Eval.inputDoc)
                        _                 -> eopts

                Interpreter.evalExpr i eopts' pkgname expr
            forM_ mbRows $ \rows -> case rows of
                [] -> PP.hPutSemDoc IO.stderr $ PP.pretty Eval.emptyObject
                _  -> forM_ rows $ \row ->
                    PP.hPutSemDoc IO.stdout $ "=" <+> PP.pretty row

        Nothing -> return ()

-- | Only makes sense in stepping mode.  Otherwise, does nothing.
processStep :: Handle -> StepTo -> Eval.StepState Eval.Value -> IO ()
processStep h stepTo state = do
    mbStep <- runInterpreter h $ \i -> Interpreter.step i state
    case mbStep of
        Nothing                      ->
            PP.hPutSemDoc IO.stdout $ prefix "internal error"
        Just Interpreter.Done        -> do
            PP.hPutSemDoc IO.stdout $ prefix "finished"
            IORef.writeIORef (h ^. mode) RegularMode
        Just (Interpreter.Yield x nstate)   -> do
            PP.hPutSemDoc IO.stdout $ prefix "=" <+> PP.pretty x
            processStep h stepTo nstate
        Just (Interpreter.Suspend source nstate) -> do
            maybeCont <- continueStepping stepTo
                (source, nstate ^. Eval.ssEnvironment . Eval.stack)
            case maybeCont of
                Just cont -> processStep h cont nstate
                Nothing -> do
                    sauce <- IORef.readIORef (h ^. sources)
                    PP.hPutSemDoc IO.stdout $ prettySnippet sauce source
                    IORef.writeIORef (h ^. mode) (Suspended source nstate)
        Just (Interpreter.Error env ctx e)   -> do
            PP.hPutSemDoc IO.stdout $ prefix "error"
            sauce <- IORef.readIORef (h ^. sources)
            Error.hPutErrors IO.stderr sauce Error.TextFmt [e]
            IORef.writeIORef (h ^. mode) (Errored env ctx e)
  where
    prefix = (PP.hint "(debug)" <+>)

    continueStepping :: StepTo -> Suspension -> IO (Maybe StepTo)
    continueStepping (StepToBreak mbOldStack) suspension@(_, stack)
        | Just stack == mbOldStack = return $ Just (StepToBreak mbOldStack)
        | otherwise                = do
            bkpnts <- IORef.readIORef (h ^. breakpoints)
            let shouldBreak = isBreakpoint suspension bkpnts
            return $ if shouldBreak then Nothing else Just $ StepToBreak mbOldStack

    continueStepping StepInto _ = return Nothing

    continueStepping (StepOver oldStack) (_, stack)
        | Stack.isStepOver stack oldStack = return Nothing
        | otherwise                       = return $ Just (StepOver oldStack)

prettySnippet :: Sources.Sources -> SourceSpan -> PP.SemDoc
prettySnippet sauce loc = case SourceSpan.citeSourceSpan PP.hint sauce loc of
    Nothing -> "at" <+> PP.pretty loc
    Just d  -> d

prettySuspension :: Sources.Sources -> Suspension -> PP.SemDoc
prettySuspension sauce (loc, stack) = PP.vcat2 $
    [prettySnippet sauce loc] ++
    (if Stack.null stack
        then []
        else ["Stack trace:" <$$> PP.ind (PP.pretty stack)])

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
        pkg   <- readFocusedPackage h
        emode <- IORef.readIORef (h ^. mode)
        return $
            review packageNameFromString pkg <>
            (case emode of
                RegularMode   -> ""
                Suspended _ _ -> "(debug)"
                Errored _ _ _ -> "(error)") <>
            "% "

metaShortcuts :: HMS.HashMap T.Text MetaCommand
metaShortcuts =
    let base = HMS.fromList $ map (\m -> (m ^. metaName, m)) metaCommands in
    HMS.shortcuts shortcuts base
  where
    shortcuts = HMS.fromList
        [ (":h", ":help")
        , (":l", ":load")
        , (":n", ":next")
        , (":o", ":open")
        , (":q", ":quit")
        , (":r", ":reload")
        , (":s", ":step")
        , (":t", ":test")
        ]

metaCommands :: [MetaCommand]
metaCommands =
    [ MetaCommand ":break" "Set a breakpoint" $ \h args -> case args of
        [point] | Just qualify <- point ^? breakpointFromText -> do
            openPkg <- liftIO $ readFocusedPackage h
            let bpt = qualifyBreakpoint openPkg qualify
            liftIO $ IORef.atomicModifyIORef_ (h ^. breakpoints) $ HS.insert bpt
            return True

        [] -> liftIO $ do
            bpts  <- IORef.readIORef (h ^. breakpoints)
            case HS.null bpts of
                False -> forM_ bpts $
                    T.hPutStrLn IO.stdout . review breakpointFromText
                True  -> IO.hPutStrLn IO.stderr $ unlines $
                    "no breakpoints set" : "" : breakHelp

            return True

        _ -> do
            liftIO $ IO.hPutStrLn IO.stderr $ unlines breakHelp
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

    , MetaCommand ":input" "set the input document" $ \h args -> do
        case args of
            _ | [path] <- T.unpack <$> args -> liftIO $ void $
                runInterpreter h (`Interpreter.setInput` path)
            _ -> liftIO $ IO.hPutStrLn IO.stderr $
                ":input takes one path argument"
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

    , MetaCommand ":quit" "exit the repl" $ \h _ -> liftIO $ do
        oldMode <- IORef.atomicModifyIORef (h ^. mode) $ \m -> (RegularMode, m)
        case oldMode of
            RegularMode -> return False
            _           -> return True

    , MetaCommand ":reload" "reload the file from the last `:load`" $
        \h _ -> liftIO $ do
            mbLastLoad <- IORef.readIORef (h ^. lastLoad)
            case mbLastLoad of
                Just ll -> load h ll
                Nothing -> IO.hPutStrLn IO.stderr "No files loaded" $> True

    , MetaCommand ":continue" "continue running the debugged program" $
        stepWith (StepToBreak . Just . view (Eval.ssEnvironment . Eval.stack))

    , MetaCommand ":step" "step (into) the next rule in the debugged program" $
        stepWith (const StepInto)

    , MetaCommand ":next" "step (over) the next rule in the debugged program" $
        stepWith (StepOver . view (Eval.ssEnvironment . Eval.stack))

    , MetaCommand ":test" "run tests in the current package" $
        \h _ -> liftIO $ do
            pkg     <- IORef.readIORef (h ^. openPackage)
            results <- runInterpreter h $ \i -> do
                rules <- map ((,) pkg) <$> Interpreter.readPackageRules i pkg
                foldMapM (Test.runTest i) $ filter Test.isTest rules
            sauce <- IORef.readIORef (h ^. sources)
            forM_ results (Test.printTestResults IO.stdout sauce)
            return True

    , MetaCommand ":where" "print your location" $ \h _ -> liftIO $ do
        emode <- IORef.readIORef (h ^. mode)
        case emode of
            Suspended source nstep -> do
                sauce <- IORef.readIORef (h ^. sources)
                PP.hPutSemDoc IO.stdout $ prettySuspension sauce
                    (source, nstep ^. Eval.ssEnvironment . Eval.stack)
            Errored _ _ err -> do
                sauce <- IORef.readIORef (h ^. sources)
                Error.hPutErrors IO.stderr sauce Error.TextFmt [err]
            _ -> PP.hPutSemDoc IO.stderr "only available when in debugging"
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

    stepWith f = \h _ -> liftIO $ do
        emode <- IORef.readIORef (h ^. mode)
        case emode of
            RegularMode        -> IO.hPutStrLn IO.stderr "Not paused"
            Errored _ _ _      -> IO.hPutStrLn IO.stderr "Not paused"
            Suspended _  nstep -> processStep h (f nstep) nstep
        return True

    breakHelp =
        [ "You can set a breakpoint at a rule by using its full "
        , "name, e.g.:"
        , ""
        , "    :break pkg.foo.bar"
        , ""
        , "Or a source file and line number, e.g.:"
        , ""
        , "    :break foo/bar.rego:9"
        ]

completeBuiltins :: Handle -> Hl.CompletionFunc IO
completeBuiltins _h = Hl.completeDictionary completeWhitespace $ return
    [ T.unpack (nameToText fname)
    | (Prepare.NamedFunction fname, _) <- HMS.toList Builtins.builtins
    ]

completeRules :: Handle -> Hl.CompletionFunc IO
completeRules h = Hl.completeDictionary completeWhitespace $ do
    pkg     <- readFocusedPackage h
    results <- runInterpreter h $ \i -> Interpreter.readPackageRules i pkg
    return $ map varToString $ fromMaybe [] results

completePackages :: Handle -> Hl.CompletionFunc IO
completePackages h = Hl.completeDictionary completeWhitespace $ do
    pkgs <- fromMaybe [] <$> runInterpreter h Interpreter.readPackages
    return $
        map ((<> ".") . review dataPackageNameFromString) pkgs ++
        map ((<> ".") . review packageNameFromString) pkgs

completePackageRules :: Handle -> Hl.CompletionFunc IO
completePackageRules h = Hl.completeWord Nothing completeWhitespace $ \str0 -> do
    let (prefix, pkgname) =
            bimap reverse (reverse . drop 1) $
            break (== '.') (reverse str0)
        dataPrefix = "data." `L.isPrefixOf` pkgname
        mbPkgName = pkgname ^?
            (if dataPrefix
                then dataPackageNameFromString
                else packageNameFromString)
    case mbPkgName of
        Nothing      -> return []
        Just pkg -> do
            rules <- runInterpreter h $ \i -> Interpreter.readPackageRules i pkg
            return $ do
                rule <- fromMaybe [] rules
                let r = varToString rule
                    text =
                        review (if dataPrefix
                                    then dataPackageNameFromString
                                    else packageNameFromString) pkg <>
                        "." <> r
                guard $ prefix `L.isPrefixOf` r
                return (Hl.Completion text text False)

completeWhitespace :: String
completeWhitespace = "(){}=;:+-/* \t\n"
