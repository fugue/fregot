{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Repl
    ( Config (..)
    , defaultConfig
    , Handle
    , withHandle
    , run

    , MetaCommand (..), metaName, metaDescription, metaRun
    , metaCommands

    , setInputFile
    ) where

import           Control.Concurrent.MVar           (MVar)
import qualified Control.Concurrent.MVar           as MVar
import           Control.Lens                      (maximumOf, preview, review,
                                                    to, view, (^.), (^?), _1)
import           Control.Lens.TH                   (makeLenses)
import           Control.Monad                     (unless)
import           Control.Monad.Extended            (foldMapM, forM_, guard,
                                                    void, when)
import           Control.Monad.Parachute
import           Control.Monad.Trans               (liftIO)
import           Data.Bifunctor                    (bimap)
import           Data.Char                         (isSpace)
import           Data.Foldable                     (for_)
import           Data.Functor                      (($>))
import qualified Data.HashMap.Strict.Extended      as HMS
import qualified Data.HashSet                      as HS
import           Data.IORef.Extended               (IORef)
import qualified Data.IORef.Extended               as IORef
import qualified Data.List                         as L
import           Data.List.NonEmpty.Extended       (NonEmpty (..))
import qualified Data.List.NonEmpty.Extended       as NonEmpty
import           Data.Maybe                        (fromMaybe, isNothing)
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T
import           Data.Version                      (showVersion)
import qualified Fregot.Error                      as Error
import qualified Fregot.Error.Stack                as Stack
import qualified Fregot.Eval                       as Eval
import qualified Fregot.Eval.Value                 as Eval
import qualified Fregot.Interpreter                as Interpreter
import           Fregot.Names
import qualified Fregot.Parser                     as Parser
import qualified Fregot.Prepare.Ast                as Prepare
import           Fregot.PrettyPrint                ((<$$>), (<+>))
import qualified Fregot.PrettyPrint                as PP
import           Fregot.Repl.Breakpoint
import qualified Fregot.Repl.FileWatch             as FileWatch
import qualified Fregot.Repl.Multiline             as Multiline
import           Fregot.Repl.Parse
import qualified Fregot.Sources                    as Sources
import           Fregot.Sources.SourceSpan         (SourceSpan)
import qualified Fregot.Sources.SourceSpan         as SourceSpan
import           Fregot.Sugar
import qualified Fregot.Test                       as Test
import           Fregot.Version                    (version)
import qualified System.Console.Haskeline.Extended as Hl
import qualified System.Console.Haskeline.History  as Hl
import qualified System.Directory                  as Directory
import           System.FilePath                   ((</>))
import qualified System.IO.Extended                as IO

-- | We can resume from this, or go back to this suspend at a later point.
type Suspend = (SourceSpan, Eval.ResumeStep Eval.Value)

data Mode
    = RegularMode [Suspend]
    | Suspended   (NonEmpty Suspend)
    | Errored     Eval.EnvContext Error.Error [Suspend]

data StepTo
    = StepToBreak (Maybe Stack.StackTrace)
    | StepInto
    | StepOver    Stack.StackTrace

data Config = Config
    { _historyFile   :: !(Maybe FilePath)
    , _resumeHistory :: !Int
    } deriving (Show)

data Handle = Handle
    { _config        :: !Config
    , _sources       :: !Sources.Handle
    , _fileWatch     :: !FileWatch.Handle
    -- | Stored in an MVar so we have a mutex.
    , _interpreter   :: !(MVar Interpreter.Handle)
    , _replCount     :: !(IORef Int)
    -- | Currently open package.
    , _openPackage   :: !(IORef PackageName)

    -- | Current mode; either debugging or regular evaluation.
    , _mode          :: !(IORef Mode)
    , _breakpoints   :: !(IORef (HS.HashSet Breakpoint))

    -- | Stored because we need to watch this for changes.
    , _inputPath     :: !(IORef (Maybe FilePath))

    -- | Evaluate this after file changes.
    , _watchInput    :: !(IORef (Maybe T.Text))
    }

data MetaCommand = MetaCommand
    { _metaName        :: !T.Text
    , _metaDescription :: !T.Text
    , _metaRun         :: Handle -> [T.Text] -> IO Bool
    }

$(makeLenses ''Config)
$(makeLenses ''Handle)
$(makeLenses ''MetaCommand)

defaultConfig :: IO Config
defaultConfig = do
    home <- Directory.getHomeDirectory
    pure $ Config (Just $ home </> ".fregot.repl") 10

withHandle
    :: Config
    -> Sources.Handle
    -> FileWatch.Handle
    -> Interpreter.Handle
    -> (Handle -> IO a)
    -> IO a
withHandle _config _sources _fileWatch interp f = do
    _replCount   <- IORef.newIORef 0
    _interpreter <- MVar.newMVar interp
    _openPackage <- IORef.newIORef "repl"
    _mode        <- IORef.newIORef $ RegularMode []
    _breakpoints <- IORef.newIORef HS.empty
    _inputPath   <- IORef.newIORef Nothing
    _watchInput  <- IORef.newIORef Nothing

    let handle = Handle {..}
    FileWatch.listen _fileWatch $ \paths -> do
        IO.hPutStrLn IO.stdout ""
        reloaded <- reload handle paths
        when (reloaded == ReloadSuccess) $ do
            mbWatchInput <- IORef.readIORef (handle ^. watchInput)
            for_ mbWatchInput $ \input -> processLine handle input

        -- NOTE(jaspervdj): This does not work well if the user has already
        -- typed part of a prompt; we would not some way to redraw that.
        --
        -- TODO(jaspervdj): Make we can clean the line by doing Ctrl+U
        prompt <- getPrompt handle
        IO.hPutStr IO.stdout prompt
        IO.hFlush IO.stdout

    f handle

-- | Auxiliary function to invoke the interpreter.  This locks the interpreter
-- resource.
runInterpreter
    :: Handle -> (Interpreter.Handle -> Interpreter.InterpreterM a)
    -> IO (Maybe a)
runInterpreter h f = MVar.withMVar (h ^. interpreter) $ \interp -> do
    (errors, mbX) <- runParachuteT $ f interp
    sauce <- IORef.readIORef (h ^. sources)
    Error.hPutErrors IO.stderr sauce Error.Text errors
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
            RegularMode _            -> Nothing
            Errored ec _ _           -> Just $ ec ^. Eval.ecEnvironment . Eval.stack
            Suspended ((_, ss) :| _) -> Just $
                ss ^. _1 . Eval.ecEnvironment . Eval.stack
    return $ fromMaybe open (stack >>= Stack.package)

freshReplInput :: Handle -> T.Text -> IO Sources.SourcePointer
freshReplInput h input = do
    replNum <- IORef.atomicModifyIORef (h ^. replCount) $ \x -> (x + 1, x)
    let sourcep = Sources.ReplInput replNum input
    IORef.atomicModifyIORef_ (h ^. sources) $ Sources.insert sourcep input
    pure $ Sources.ReplInput replNum input

readEvalContext :: Handle -> IO (Maybe Interpreter.EnvContext)
readEvalContext h = do
    emode <- IORef.readIORef (h ^. mode)
    pure $ case emode of
        Suspended ((_, (ec, _)) :| _) -> Just ec
        Errored ec _ _                -> Just ec
        _                             -> Nothing

processLine :: Handle -> T.Text -> IO Bool
processLine h input
    | (meta : args) <- T.words input
    , ":" `T.isPrefixOf` meta
    , Just cmd <- HMS.lookup meta metaShortcuts = (cmd ^. metaRun) h args
    | otherwise                                 = processInput h input $> True

processInput :: Handle -> T.Text -> IO ()
processInput h input = do
    sourcep                   <- freshReplInput h input
    (parseErrs, mbRuleOrTerm) <- runParachuteT $ parseRuleOrQuery sourcep input
    sauce <- IORef.readIORef (h ^. sources)
    Error.hPutErrors IO.stderr sauce Error.Text parseErrs

    emode   <- IORef.readIORef (h ^. mode)
    bkpts   <- IORef.readIORef (h ^. breakpoints)
    pkgname <- readFocusedPackage h
    case mbRuleOrTerm of
        Just (Left rule) | RegularMode _ <- emode -> do
            mbResult <- runInterpreter h $ \i -> do
                Interpreter.insertRule i pkgname rule
                Interpreter.compileRules i
            unless (isNothing mbResult) $ PP.hPutSemDoc IO.stderr $
                "Rule" <+>
                PP.code (PP.pretty (rule ^. ruleHead . ruleName)) <+>
                "added"

        Just (Left _rule) ->
            -- NOTE(jaspervdj): I think it shouldn't be /too/ hard to allow
            -- this, but I'm not sure if it's worth it.
            IO.hPutStrLn IO.stderr $ "Cannot add rules while debugging"

        Just (Right expr) | not (HS.null bkpts), RegularMode _ <- emode -> do
            mbStepState <- runInterpreter h $ \i ->
                Interpreter.newResumeStep i pkgname expr

            case mbStepState of
                Nothing     -> return ()
                Just sstate -> processStep h [] (StepToBreak Nothing) sstate

        Just (Right expr) -> do
            envctx <- readEvalContext h
            mbRows <- runInterpreter h $ \i -> do
                -- Figure out what environment we want to eval in.
                Interpreter.evalQuery i envctx pkgname expr
            forM_ mbRows $ \rows -> case rows of
                [] -> PP.hPutSemDoc IO.stderr $ PP.pretty Eval.emptyObject
                _  -> forM_ rows $ \row ->
                    PP.hPutSemDoc IO.stdout $ "=" <+> PP.pretty row

        Nothing -> return ()

-- | Only makes sense in stepping mode.  Otherwise, does nothing.
processStep
    :: Handle -> [Suspend] -> StepTo -> Eval.ResumeStep Eval.Value -> IO ()
processStep h suspends stepTo resume = do
    mbStep <- runInterpreter h $ \i -> Interpreter.step i resume
    case mbStep of
        Nothing                      ->
            PP.hPutSemDoc IO.stdout $ prefix "internal error"
        Just Interpreter.Done        -> do
            PP.hPutSemDoc IO.stdout $ prefix "finished"
            IORef.writeIORef (h ^. mode) $ RegularMode suspends
        Just (Interpreter.Yield x ec cont)   -> do
            PP.hPutSemDoc IO.stdout $ prefix "=" <+> PP.pretty x
            processStep h suspends stepTo (ec, cont)
        Just (Interpreter.Suspend source ec cont) -> do
            mbNextStepTo <- continueStepping stepTo
                (source, ec ^. Eval.ecEnvironment . Eval.stack)
            case mbNextStepTo of
                Just nextStepTo -> processStep h suspends nextStepTo (ec, cont)
                Nothing         -> do
                    sauce <- IORef.readIORef (h ^. sources)
                    PP.hPutSemDoc IO.stdout $ prettySnippet sauce source
                    IORef.writeIORef (h ^. mode) $ Suspended $
                        (source, (ec, cont)) :| take (h ^. config . resumeHistory) suspends
        Just (Interpreter.Error envctx e)   -> do
            PP.hPutSemDoc IO.stdout $ prefix "error"
            sauce <- IORef.readIORef (h ^. sources)
            Error.hPutErrors IO.stderr sauce Error.Text [e]
            IORef.writeIORef (h ^. mode) (Errored envctx e suspends)
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

    let settings = Hl.Settings
            { Hl.historyFile    = h ^. config . historyFile
            , Hl.autoAddHistory = False
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
    -- Unfortunately we don't have good multiline history editing, so we're
    -- stuck with adding the lines one by one.
    addHistory :: T.Text -> Hl.InputT IO ()
    addHistory txt = forM_ (filter (not . T.null) $ T.lines txt) $ \l ->
        Hl.modifyHistory $ Hl.addHistoryRemovingAllDupes $ T.unpack l

    loop :: Hl.InputT IO ()
    loop = do
        mbInput <- getMultilineInput
        case mbInput of
            Nothing -> return ()
            Just input | T.all isSpace input ->
                loop
            Just input -> do
                addHistory input
                cont <- liftIO $ processLine h input
                when cont loop

    getMultilineInput :: Hl.InputT IO (Maybe T.Text)
    getMultilineInput = do
        prompt  <- liftIO (getPrompt h)
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

getPrompt :: Handle -> IO String
getPrompt h = do
    pkg   <- readFocusedPackage h
    emode <- IORef.readIORef (h ^. mode)
    return $
        review packageNameFromString pkg <>
        (case emode of
            RegularMode _ -> ""
            Suspended _   -> "(debug)"
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
    [ MetaCommand ":break" "Set or remove a breakpoint" $ \h args -> case args of
        [point] | Just qualify <- point ^? breakpointFromText -> do
            openPkg <- readFocusedPackage h
            let bpt = qualifyBreakpoint openPkg qualify
            verb <- IORef.atomicModifyIORef' (h ^. breakpoints) $ \bpts ->
                if HS.member bpt bpts
                    then (HS.delete bpt bpts, "Removed")
                    else (HS.insert bpt bpts, "Set")
            T.hPutStrLn IO.stderr $
                verb <> " breakpoint at " <> review breakpointFromText bpt
            return True

        [] -> do
            bpts  <- IORef.readIORef (h ^. breakpoints)
            case HS.null bpts of
                False -> forM_ bpts $
                    T.hPutStrLn IO.stdout . review breakpointFromText
                True  -> IO.hPutStrLn IO.stderr $ unlines $
                    "no breakpoints set" : "" : breakHelp

            return True

        _ -> do
            IO.hPutStrLn IO.stderr $ unlines breakHelp
            return True

    , MetaCommand ":help" "show this info" $ \_ _ -> do
        let width   = maximumOf (traverse . metaName . to T.length) metaCommands
            justify = T.justifyLeft (fromMaybe 0 width + 2) ' '
        PP.hPutSemDoc IO.stderr $
            "Enter an expression to evaluate it." <$$>
            "Enter a rule to add it to the current package." <$$>
            mempty <$$>
            "Other commands:" <$$>
            (PP.ind $ PP.vcat $ do
                MetaCommand {..} <- metaCommands
                return $ PP.code (PP.pretty $ justify _metaName) <>
                    PP.pretty _metaDescription) <$$>
            mempty <$$>
            "Shortcuts are supported for commands, e.g. `:l` for `:load`."
        return True

    , MetaCommand ":input" "set the input document" $ \h args -> do
        case args of
            _ | [path] <- T.unpack <$> args -> setInputFile h path
            _ -> IO.hPutStrLn IO.stderr ":input takes one path argument"
        return True

    , MetaCommand ":open" "open a different package, e.g. `:open foo`" $
        \h args -> case map (preview dataPackageNameFromText) args of
            [Just (_, pkg)] -> do
                -- NOTE(jaspervdj): Rather than erroring if it doesn't exist, we
                -- just make it clear that the user has opened a new package.
                pkgs <- runInterpreter h Interpreter.readPackages
                let exists = maybe False (pkg `elem`) pkgs
                unless exists $ PP.hPutSemDoc IO.stderr $
                    "Created new package" <+> PP.code (PP.pretty pkg)
                IORef.writeIORef (h ^. openPackage) pkg $> True
            _ -> do
                IO.hPutStrLn IO.stderr ":open takes a package name as argument"
                return True

    , MetaCommand ":quit" "exit the repl" $ \h _ -> do
        oldMode <- IORef.atomicModifyIORef (h ^. mode) $
            \m -> (RegularMode [], m)
        case oldMode of
            RegularMode _ -> return False
            _             -> return True

    , MetaCommand ":load" "load a rego file, e.g. `:load foo.rego`" $ \h args ->
        case map T.unpack args of
        [path] -> do
            IO.hPutStrLn IO.stderr $ "Loading " ++ path ++ "..."
            FileWatch.watch (h ^. fileWatch) path
            void $ runInterpreter h $ \i -> do
                pkg <- Interpreter.loadModule i Parser.defaultParserOptions path
                Interpreter.compileRules i
                liftIO $ IO.hPutStrLn IO.stderr $
                    "Loaded package " ++ review packageNameFromString pkg
                liftIO $ IORef.writeIORef (h ^. openPackage) pkg
            return True

        _ -> do
            IO.hPutStrLn IO.stderr ":load takes one path argument"
            return True

    , MetaCommand ":reload" "reload modified rego files" $
        \h _ -> do
            paths <- FileWatch.pop (h ^. fileWatch)
            reloaded <- reload h paths
            when (reloaded == ReloadRefusedDebug) $ liftIO $
                IO.hPutStrLn IO.stderr $
                "Reloading when debugging is not possible as it would " ++
                "modify the code currently running."
            pure True

    , MetaCommand ":continue" "continue running the debugged program" $
        stepWith (StepToBreak . Just . view (_1 . Eval.ecEnvironment . Eval.stack))

    , MetaCommand ":step" "step (into) the next rule in the debugged program" $
        stepWith (const StepInto)

    , MetaCommand ":next" "step (over) the next rule in the debugged program" $
        stepWith (StepOver . view (_1 . Eval.ecEnvironment . Eval.stack))

    , MetaCommand ":rewind" "go back to the previous debug suspension" $ do
        \h _ -> do
            sauce  <- IORef.readIORef (h ^. sources)
            source <- IORef.atomicModifyIORef' (h ^. mode) $ \case
                RegularMode (resume : resumes) ->
                    (Suspended (resume :| resumes), Just (fst resume))
                Suspended (_ :| resume : resumes) ->
                    (Suspended (resume :| resumes), Just (fst resume))
                Errored _ _ (resume : resumes) ->
                    (Suspended (resume :| resumes), Just (fst resume))
                emode -> (emode, Nothing)
            case source of
                Just s  -> PP.hPutSemDoc IO.stdout $ prettySnippet sauce s
                Nothing -> IO.hPutStrLn IO.stderr $
                    "No previous suspension point in the debugger"
            return True

    , MetaCommand ":test" "run tests in the current package" $
        \h _ -> do
            pkg     <- IORef.readIORef (h ^. openPackage)
            results <- runInterpreter h $ \i -> do
                rules <- map ((,) pkg) <$> Interpreter.readPackageRules i pkg
                foldMapM (Test.runTest i) $ filter Test.isTest rules
            sauce <- IORef.readIORef (h ^. sources)
            forM_ results (Test.printTestResults IO.stdout sauce)
            return True

    , MetaCommand ":type" "print the type of a term" $ \h args -> liftIO $ do
        let input = T.unwords args
        sourcep <- freshReplInput h input
        (errs, mbExpr) <- runParachuteT $
            Parser.lexAndParse Parser.expr sourcep input

        sauce <- IORef.readIORef (h ^. sources)
        Error.hPutErrors IO.stderr sauce Error.Text errs

        pkgname <- IORef.readIORef (h ^. openPackage)
        forM_ mbExpr $ \expr -> do
            envctx <- readEvalContext h
            mbType <- runInterpreter h $ \i ->
                Interpreter.typeExpr i envctx pkgname expr
            forM_ mbType $ \ty -> PP.hPutSemDoc IO.stdout $
                PP.pretty expr <+> PP.punctuation ":" <+> PP.pretty ty

        return True

    , MetaCommand ":where" "print your location" $ \h _ -> do
        emode <- IORef.readIORef (h ^. mode)
        case emode of
            Suspended ((source, (ec, _)) :| _) -> do
                sauce <- IORef.readIORef (h ^. sources)
                PP.hPutSemDoc IO.stdout $ prettySuspension sauce
                    (source, ec ^. Eval.ecEnvironment . Eval.stack)
            Errored _ err _ -> do
                sauce <- IORef.readIORef (h ^. sources)
                Error.hPutErrors IO.stderr sauce Error.Text [err]
            _ -> PP.hPutSemDoc IO.stderr "only available when in debugging"
        return True

    , MetaCommand ":watch" "evaluate input after file changes" $
        \h args -> do
        if FileWatch.listenersEnabled (h ^. fileWatch)
            then
                let input = T.unwords args in
                IORef.writeIORef (h ^. watchInput) $
                    guard (not $ T.all isSpace input) $> input
            else
                IO.hPutStrLn IO.stderr $
                    "Restart the with `--watch` to enable watching."
        return True
    ]
  where
    stepWith f = \h _ -> do
        emode <- IORef.readIORef (h ^. mode)
        case emode of
            RegularMode _ -> IO.hPutStrLn IO.stderr "Not paused"
            Errored _ _ _ -> IO.hPutStrLn IO.stderr "Not paused"
            Suspended resumes@((_, nstep) :| _) ->
                processStep h (NonEmpty.toList resumes) (f nstep) nstep
        return True

    breakHelp =
        [ "You can set a breakpoint at a rule by using its full name, e.g.:"
        , ""
        , "    :break pkg.foo.bar"
        , ""
        , "Or a source file and line number, e.g.:"
        , ""
        , "    :break foo/bar.rego:9"
        , ""
        , "You can remove breakpoints with the same command."
        ]

data Reload
    = ReloadError
    | ReloadSuccess
    | ReloadRefusedDebug
    deriving (Eq)

reload :: Handle -> [FilePath] -> IO Reload
reload h paths = guarded $ runInterpreter h $ \i -> do
    -- Separate input and normal paths.
    mbInput <- liftIO $ IORef.readIORef (h ^. inputPath)
    let (regoPaths, inputPaths) = L.partition ((/= mbInput) . Just) $ paths

    unless (null inputPaths) $ for_ mbInput $ \input -> do
        Interpreter.setInputFile i input
        liftIO $ IO.hPutStrLn IO.stderr $ "Reloaded " ++ input

    forM_ regoPaths $ \path ->
        Interpreter.loadModule i Parser.defaultParserOptions path
    Interpreter.compileRules i
    liftIO $ case regoPaths of
        []     -> pure ()
        [path] -> IO.hPutStrLn IO.stderr $ "Reloaded " ++ path
        _ : _  -> IO.hPutStrLn IO.stderr $
            "Reloaded " ++ show (length regoPaths) ++ " files"
  where
    guarded reloader =
        IORef.readIORef (h ^. mode) >>= \case
            Suspended   _ -> pure ReloadRefusedDebug
            Errored _ _ _ -> pure ReloadRefusedDebug
            RegularMode _ ->
                maybe ReloadError (const ReloadSuccess) <$> reloader

-- | Wraps `Interpreter.setInputFile` and takes care of file watching.
setInputFile :: Handle -> FilePath -> IO ()
setInputFile h path = do
    mbOld <- IORef.readIORef (h ^. inputPath)
    for_ mbOld $ \old -> FileWatch.unwatch (h ^. fileWatch) old
    void $ runInterpreter h (`Interpreter.setInputFile` path)
    IORef.writeIORef (h ^. inputPath) (Just path)
    FileWatch.watch (h ^. fileWatch) path

completeBuiltins :: Handle -> Hl.CompletionFunc IO
completeBuiltins h = Hl.completeDictionary completeWhitespace $ do
    builtins <- fromMaybe [] <$> runInterpreter h Interpreter.readBuiltins
    return [T.unpack (nameToText f) | Prepare.NamedFunction f <- builtins]

completeRules :: Handle -> Hl.CompletionFunc IO
completeRules h = Hl.completeDictionary completeWhitespace $ do
    pkg     <- readFocusedPackage h
    results <- runInterpreter h $ \i -> Interpreter.readPackageRules i pkg
    return $ map varToString $ fromMaybe [] results

completePackages :: Handle -> Hl.CompletionFunc IO
completePackages h = Hl.completeDictionary completeWhitespace $ do
    pkgs <- fromMaybe [] <$> runInterpreter h Interpreter.readPackages
    return $ do
        pkg        <- pkgs
        dataPrefix <- [True, False]
        pure $ review dataPackageNameFromString (dataPrefix, pkg) <> "."

completePackageRules :: Handle -> Hl.CompletionFunc IO
completePackageRules h = Hl.completeWord Nothing completeWhitespace $ \str0 -> do
    let (prefix, pkgname) =
            bimap reverse (reverse . drop 1) $
            break (== '.') (reverse str0)
        mbPkgName = pkgname ^? dataPackageNameFromString
    case mbPkgName of
        Nothing      -> return []
        Just (dataPrefix, pkg) -> do
            rules <- runInterpreter h $ \i -> Interpreter.readPackageRules i pkg
            return $ do
                rule <- fromMaybe [] rules
                let r = varToString rule
                    text =
                        review dataPackageNameFromString (dataPrefix, pkg) <>
                        "." <> r
                guard $ prefix `L.isPrefixOf` r
                return (Hl.Completion text text False)

completeWhitespace :: String
completeWhitespace = "(){}=;:+-/* \t\n"
