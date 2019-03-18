{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Main
    ( main
    ) where

import           Control.Applicative       ((<|>))
import           Control.Concurrent        (threadDelay)
import qualified Control.Concurrent.Async  as Async
import qualified Control.Concurrent.MVar   as MVar
import           Control.Exception         (Exception, throwIO)
import           Control.Monad.Extended    (forM, forM_, forever, ifM, mzero,
                                            unless, when)
import qualified Data.Aeson.Encode.Pretty  as Aeson.Pretty
import qualified Data.Aeson.Extended       as A
import           Data.Algorithm.Diff
import           Data.Algorithm.DiffOutput
import qualified Data.ByteString.Extended  as B
import qualified Data.ByteString.Lazy      as BL
import           Data.Function             (on)
import qualified Data.IORef                as IORef
import qualified Data.List                 as List
import           Data.Maybe                (fromMaybe)
import           Data.Semigroup            (Semigroup (..))
import qualified Data.Text                 as T
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as T
import qualified Data.Text.Encoding        as Text
import           Data.Time                 (NominalDiffTime, diffUTCTime,
                                            getCurrentTime)
import           Data.Typeable             (Typeable)
import           Data.Version              (showVersion)
import           Fregot.Version            (version)
import qualified Options.Applicative       as OA
import           System.Directory          (doesDirectoryExist, doesFileExist,
                                            removeDirectoryRecursive,
                                            removeFile, withCurrentDirectory)
import qualified System.Directory.Find     as Find
import           System.Environment        (getEnvironment)
import           System.Exit               (ExitCode (..), exitFailure)
import           System.FilePath           (dropExtension, isAbsolute,
                                            normalise, takeBaseName,
                                            takeDirectory, (</>))
import qualified System.IO                 as IO
import qualified System.Process            as Process
import qualified Text.Pcre2                as Pcre2
import           Text.Printf               (printf)

--------------------------------------------------------------------------------

type SpliceEnv = [(String, String)]

data MissingEnvVar = MissingEnvVar String
    deriving (Typeable)

instance Show MissingEnvVar where
    show (MissingEnvVar k) = "Missing environment variable: " ++ k

instance Exception MissingEnvVar

-- | Splice in a string with "Hello ${FOO}" syntax.
splice :: SpliceEnv -> String -> Either MissingEnvVar String
splice env = go
  where
    go str = case break (== '$') str of
        (xs, '$' : '{' : ys) -> case break (== '}') ys of
            (key, '}' : zs) -> case lookup key env of
                Nothing  -> Left $ MissingEnvVar key
                Just val -> fmap ((xs ++ val) ++) (go zs)
            (_, _)          -> fmap ((xs ++ "${") ++) (go ys)
        (xs, ys) -> Right $ xs ++ ys

--------------------------------------------------------------------------------

data Spec = Spec
    { sInputFiles :: !(Maybe String)
    , sCommand    :: !String
    , sArguments  :: ![String]
    , sStdin      :: ![String]
    , sEnv        :: ![(String, String)]
    , sAsserts    :: ![Assert]
    }

instance A.FromJSON Spec where
    parseJSON = A.withObject "FromJSON Spec" $ \o -> Spec
        <$> o A..:? "input_files"
        <*> o A..:  "command"
        <*> o A..:  "arguments"
        <*> (maybe [] A.unMultiple <$> o A..:? "stdin")
        <*> (fromMaybe [] <$> o A..:? "environment")
        <*> o A..:  "asserts"

--------------------------------------------------------------------------------

newtype PostProcess = PostProcess [PostProcessStep]
    deriving (Monoid, Semigroup)

instance A.FromJSON PostProcess where
    parseJSON val@(A.Array _) = PostProcess <$> A.parseJSON val
    parseJSON val             = PostProcess . return <$> A.parseJSON val

data PostProcessStep
    = PrettifyJsonStep
    | FixWindowsNewlinesStep
    | FixWindowsPathsStep
    | ReplaceStep !Pcre2.Regex !T.Text

instance A.FromJSON PostProcessStep where
    parseJSON (A.String s) = case s of
        "prettify_json"        -> pure PrettifyJsonStep
        "fix_windows_newlines" -> pure FixWindowsNewlinesStep
        "fix_windows_paths"    -> pure FixWindowsPathsStep
        _                      -> fail $ "Unknown PostProcessStep: " ++ show s

    parseJSON (A.Object o) = ReplaceStep
        <$> (do
                p <- o A..: "pattern"
                either (fail . show) return (Pcre2.compileWith copts p))
        <*> o A..: "replacement"
      where
        copts = Pcre2.defaultCompileOptions {Pcre2.coMultiline = True}

    parseJSON _ = mzero

postProcess :: PostProcess -> B.ByteString -> B.ByteString
postProcess (PostProcess ps) bs0 = List.foldl' (flip postProcessStep) bs0 ps

postProcessStep :: PostProcessStep -> B.ByteString -> B.ByteString
postProcessStep PrettifyJsonStep bs = maybe bs
    (BL.toStrict . Aeson.Pretty.encodePretty' prettyConfig)
    (A.decodeStrict bs :: Maybe A.Value)
  where
    prettyConfig = Aeson.Pretty.defConfig
        { Aeson.Pretty.confIndent  = (Aeson.Pretty.Spaces 2)
        , Aeson.Pretty.confCompare = compare
        }
postProcessStep FixWindowsNewlinesStep bs0 = B.replace "\r\n" "\n" bs0
postProcessStep FixWindowsPathsStep bs0 = B.replace "\\" "/" bs0

postProcessStep (ReplaceStep regex replacement) bs =
    let text = T.decodeUtf8 bs in
    case Pcre2.match regex text of
        Left  _       -> bs
        Right matches -> T.encodeUtf8 $ go 0 text matches
  where
    go !_   remainder [] = remainder
    go !idx remainder (Pcre2.Match range _ : matches) =
        let !offset = Pcre2.rStart range - idx
            !end    = offset + Pcre2.rLength range in
        T.take offset remainder <>
        replacement <>
        go (idx + end) (T.drop end remainder) matches

--------------------------------------------------------------------------------

data Assert
    = ExitCodeAssert !Int
    | StdoutAssert
        { stdoutFilePath    :: !FilePath
        , stdoutPostProcess :: !PostProcess
        }
    | StderrAssert
        { stderrFilePath    :: !FilePath
        , stderrPostProcess :: !PostProcess
        }
    | CreatedFileAssert
        { createdFilePath        :: !FilePath
        , createdFileContents    :: !(Maybe FilePath)
        , createdFilePostProcess :: !PostProcess
        }
    | CreatedDirectoryAssert
        { createdDirectoryPath   :: !FilePath
        }

instance A.FromJSON Assert where
    parseJSON = A.withObject "FromJSON Assert" $ \o ->
        (ExitCodeAssert <$> o A..: "exit_code") <|>
        (StdoutAssert
            <$> o A..:  "stdout"
            <*> o A..:? "post_process" A..!= mempty) <|>
        (StderrAssert
            <$> o A..:  "stderr"
            <*> o A..:? "post_process" A..!= mempty) <|>
        (CreatedFileAssert
            <$> o A..:  "created_file"
            <*> o A..:? "contents"
            <*> o A..:? "post_process" A..!= mempty) <|>
        (CreatedDirectoryAssert
            <$> o A..:  "created_directory")

describeAssert :: Assert -> String
describeAssert (ExitCodeAssert     _)     = "exit_code"
describeAssert (StdoutAssert       _ _)   = "stdout"
describeAssert (StderrAssert       _ _)   = "stderr"
describeAssert (CreatedFileAssert  _ _ _) = "created_file"
describeAssert (CreatedDirectoryAssert _) = "created_directory"

spliceAssert :: SpliceEnv -> Assert -> Either MissingEnvVar Assert
spliceAssert _   (ExitCodeAssert n) = return $ ExitCodeAssert n
spliceAssert env (StdoutAssert fp pp) =
    StdoutAssert <$> splice env fp <*> pure pp
spliceAssert env (StderrAssert fp pp) =
    StderrAssert <$> splice env fp <*> pure pp
spliceAssert env (CreatedFileAssert fp fc pp) =
    CreatedFileAssert <$> splice env fp <*> traverse (splice env) fc <*> pure pp

spliceAssert env (CreatedDirectoryAssert fp) =
    CreatedDirectoryAssert <$> splice env fp

--------------------------------------------------------------------------------

data Verbosity = Debug | Message | Error
    deriving (Eq, Ord)

type Logger = Verbosity -> [String] -> IO ()

makeLogger :: Bool -> IO Logger
makeLogger verbose = do
    lock <- MVar.newMVar ()
    return $ \verbosity msgs ->
        unless (not verbose && verbosity == Debug) $
            MVar.withMVar lock $ \() -> mapM_ (IO.hPutStrLn IO.stderr) msgs

--------------------------------------------------------------------------------

data Execution = Execution
    { executionInputFile :: Maybe FilePath
    , executionCommand   :: FilePath
    , executionArguments :: [String]
    , executionStdin     :: [String]
    , executionAsserts   :: [Assert]
    , executionSpliceEnv :: SpliceEnv
    , executionSpecPath  :: FilePath
    , executionSpecName  :: String
    , executionDirectory :: FilePath
    }

specExecutions :: FilePath -> Spec -> IO [Execution]
specExecutions specPath spec = do
    let specBaseName  = takeBaseName  specPath
        specDirectory = takeDirectory specPath
        specName      = dropExtension specBaseName

    -- Compute initial environment to get input files.
    env0 <- getEnvironment
    let env1 =
            List.nubBy ((==) `on` fst) $
                ("SPEC_NAME", specName) : (sEnv spec ++ env0)

    -- Get a list of concrete input files (a list maybes).
    concreteInputFiles <- case sInputFiles spec of
        Nothing    -> return [Nothing]
        Just glob0 -> do
            glob <- hoistEither $ splice env1 glob0
            inputFiles <- withCurrentDirectory specDirectory $ do
                matches <- Find.glob glob
                length matches `seq` return matches
            return (map (Just . normalise) inputFiles)

    -- Create an execution for every concrete input.
    forM concreteInputFiles $ \mbInputFile -> do
        -- Extend environment.
        let env2 = case mbInputFile of
                Nothing        -> env1
                Just inputFile ->
                    ("SPEC_INPUT_FILE", inputFile) :
                    ("SPEC_INPUT_NAME", dropExtension inputFile) :
                    env1

        -- Return execution after doing some splicing.
        hoistEither $ do
            let executionInputFile = mbInputFile
            executionCommand   <- splice env2 (sCommand spec)
            executionArguments <- traverse (splice env2) (sArguments spec)
            executionStdin     <- traverse (splice env2) (sStdin spec)
            executionAsserts   <- traverse (spliceAssert env2) (sAsserts spec)
            let executionSpliceEnv = env2
                executionSpecPath  = specPath
                executionSpecName  = specName
                executionDirectory = specDirectory
            return Execution {..}
  where
    hoistEither :: Either MissingEnvVar a -> IO a
    hoistEither = either throwIO return

executionHeader :: Execution -> String
executionHeader execution =
    executionSpecPath execution ++
    case executionInputFile execution of
        Nothing -> ": "
        Just fp -> " (" ++ fp ++ "): "


--------------------------------------------------------------------------------

data Env = Env
    { envLogger        :: !Logger
    , envDiff          :: !Bool
    , envPrettyDiff    :: !Bool
    , envFix           :: !Bool
    , envCountAsserts  :: !(IORef.IORef Int)
    , envCountFailures :: !(IORef.IORef Int)
    }

incrementCount :: IORef.IORef Int -> IO ()
incrementCount ref = IORef.atomicModifyIORef' ref (\x -> (x + 1, ()))

data ExecutionResult = ExecutionResult
    { erExitCode :: !ExitCode
    , erStdout   :: !B.ByteString
    , erStderr   :: !B.ByteString
    } deriving (Show)

runExecution
    :: Env -> Execution -> IO ()
runExecution env execution@Execution {..} = do
    envLogger env Debug [executionHeader execution ++ "running..."]

    -- Create a "CreateProcess" description.
    let createProcess = (Process.proc executionCommand executionArguments)
            { Process.env     = Just executionSpliceEnv
            , Process.cwd     = Just executionDirectory
            , Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.CreatePipe
            }

    -- Actually run the process.
    envLogger env Debug [executionHeader execution ++
        executionCommand ++ " " ++ unwords executionArguments]
    (Just hIn, Just hOut, Just hErr, hProc) <-
        Process.createProcess createProcess

    let writeStdin = mapM_ (IO.hPutStrLn hIn) executionStdin >> IO.hClose hIn
    Async.withAsync writeStdin $ \_ ->
        Async.withAsync (B.hGetContents hOut) $ \outAsync ->
        Async.withAsync (B.hGetContents hErr) $ \errAsync ->
        Async.withAsync (Process.waitForProcess hProc) $ \exitAsync -> do

        -- Get output.
        !exitCode  <- Async.wait exitAsync
        !actualOut <- Async.wait outAsync
        !actualErr <- Async.wait errAsync
        let executionResult = ExecutionResult
                { erExitCode = exitCode
                , erStdout   = actualOut
                , erStderr   = actualErr
                }

        -- Dump stderr/stdout if in debug.
        envLogger env Debug [executionHeader execution ++ "finished"]
        envLogger env Debug [executionHeader execution ++ "stdout:", show actualOut]
        envLogger env Debug [executionHeader execution ++ "stderr:", show actualErr]

        -- Perform checks.
        envLogger env Debug [executionHeader execution ++ "checking assertions..."]
        forM_ executionAsserts $ runAssert env execution executionResult
        envLogger env Debug [executionHeader execution ++ "done"]

--------------------------------------------------------------------------------

-- | Check an assertion.
runAssert :: Env -> Execution -> ExecutionResult -> Assert -> IO ()
runAssert env execution@Execution {..} ExecutionResult {..} assert =
    case assert of
        ExitCodeAssert expectedExitCode -> do
            let actualExitCode = case erExitCode of
                    ExitSuccess   -> 0
                    ExitFailure c -> c
            assertTrue (actualExitCode == expectedExitCode) $
                "expected " ++ show expectedExitCode ++
                " but got " ++ show actualExitCode

        StdoutAssert {..} -> checkAgainstFile
            (inExecutionDir stdoutFilePath) stdoutPostProcess erStdout

        StderrAssert {..} -> checkAgainstFile
            (inExecutionDir stderrFilePath) stderrPostProcess erStderr

        CreatedFileAssert {..} -> do
            let path = inExecutionDir createdFilePath
            exists <- doesFileExist path
            assertTrue exists $ createdFilePath ++ " was not created"
            when exists $ do
                case createdFileContents of
                    Nothing           -> return ()
                    Just expectedPath -> do
                        !actual <- readFileOrEmpty path
                        checkAgainstFile
                            (inExecutionDir expectedPath)
                            createdFilePostProcess actual
                removeFile path
                envLogger env Debug [executionHeader execution ++
                    "removed " ++ createdFilePath]

        CreatedDirectoryAssert {..} -> do
            let path = inExecutionDir createdDirectoryPath
            exists <- doesDirectoryExist path
            assertTrue exists $ createdDirectoryPath ++ " was not created"
            when exists $ do
                removeDirectoryRecursive path
                envLogger env Debug [executionHeader execution ++
                    "removed " ++ createdDirectoryPath]
  where
    inExecutionDir :: FilePath -> FilePath
    inExecutionDir fp = if isAbsolute fp then fp else executionDirectory </> fp

    checkAgainstFile :: FilePath -> PostProcess -> B.ByteString -> IO ()
    checkAgainstFile expectedPath processor actual0 = do
        expected <- readFileOrEmpty expectedPath
        let !actual1 = postProcess processor actual0
        assertTrue (actual1 == expected) "does not match"
        when (envDiff env && actual1 /= expected) $ do
            envLogger env Message
                [ executionHeader execution ++ "expected:"
                , show expected
                , executionHeader execution ++ "actual:"
                , show actual1
                ]
        let diff :: [Diff [String]] = either (const []) id $ do
                expected' <- Text.unpack <$> Text.decodeUtf8' expected
                actual1'  <- Text.unpack <$> Text.decodeUtf8' actual1
                return $
                    getGroupedDiff
                        (lines expected')
                        (lines actual1')
        when (envPrettyDiff env && actual1 /= expected && not (null diff)) $ do
            envLogger env Message
                [ executionHeader execution ++ "diff:"
                , ppDiff diff
                ]
        when (envFix env && actual1 /= expected) $ do
            B.writeFile expectedPath actual1
            envLogger env Debug
                [executionHeader execution ++ "fixed " ++ expectedPath]

    assertTrue :: Bool -> String -> IO ()
    assertTrue test err = do
        incrementCount (envCountAsserts env)
        if test
            then
                envLogger env Debug [executionHeader execution ++
                    describeAssert assert ++ ": OK"]
            else do
                envLogger env Error [executionHeader execution ++
                    describeAssert assert ++ ": " ++ err]
                incrementCount (envCountFailures env)

readFileOrEmpty :: FilePath -> IO B.ByteString
readFileOrEmpty fp = do
    exists <- doesFileExist fp
    if exists then B.readFile fp else return B.empty

--------------------------------------------------------------------------------

-- | Recursively finds all '.spec' files in bunch of files or directories.
findSpecs :: [FilePath] -> IO [FilePath]
findSpecs fps = fmap concat $ forM fps $ \fp -> ifM
    (doesDirectoryExist fp)
        (map (fp </>) <$> Find.recursivelyFindFilesWithExtension ".spec" fp)
        (return [fp])


--------------------------------------------------------------------------------

data Options = Options
    { oPaths      :: [FilePath]
    , oVerbose    :: Bool
    , oDiff       :: Bool
    , oPrettyDiff :: Bool
    , oFix        :: Bool
    , oJobs       :: Int
    }

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.some (OA.strArgument (
            OA.metavar "PATH" <>
            OA.help    "Test files/directories"))
    <*> OA.switch (
            OA.short   'v' <>
            OA.help    "Be more verbose")
    <*> OA.switch (
            OA.long    "diff" <>
            OA.help    "Show differences in files")
    <*> OA.switch (
            OA.long    "pretty-diff" <>
            OA.help    "Show differences in files, output in patch format")
    <*> OA.switch (
            OA.long    "fix" <>
            OA.help    "Attempt to fix broken tests")
    <*> OA.option OA.auto (
            OA.long    "jobs" <>
            OA.short   'j'    <>
            OA.value   1      <>
            OA.help    "Number of worker jobs")

parserInfo :: OA.ParserInfo Options
parserInfo = OA.info (OA.helper <*> parseOptions) $
    OA.fullDesc <>
    OA.header ("fspec v" <> showVersion version)

--------------------------------------------------------------------------------

-- | Spawn a worker thread that takes workloads from a shared pool.
worker
    :: IORef.IORef [a]                         -- ^ Ref to a pool of work
    -> (a -> IO ())                            -- ^ Worker function
    -> IO ()
worker pool f = do
    mbWorkload <- IORef.atomicModifyIORef' pool $ \case
        []       -> ([], Nothing)
        (x : xs) -> (xs, Just x)
    case mbWorkload of
        Nothing       -> return ()
        Just workload -> f workload >> worker pool f

main :: IO ()
main = do
    startTime <- getCurrentTime
    options   <- OA.execParser parserInfo
    env       <- Env
        <$> makeLogger (oVerbose options)
        <*> pure (oDiff options)
        <*> pure (oPrettyDiff options)
        <*> pure (oFix options)
        <*> IORef.newIORef 0
        <*> IORef.newIORef 0

    -- Find all specs and decode them.
    specPaths <- findSpecs (oPaths options)
    specs     <- forM specPaths $ \specPath -> do
        !errOrSpec <- A.eitherDecodeStrict <$> B.readFile specPath
        case errOrSpec of
            Right !spec -> return (specPath, spec)
            Left  !err  -> do
                envLogger env Error
                    [specPath ++ ": could not parse JSON: " ++ err]
                exitFailure

    -- Each spec might produce a number of executions.  We can't really
    -- parallelize this because 'specExecutions' needs to change the working
    -- directory all the time and that might mess with our tests.
    let numSpecs = length specs
    envLogger env Message ["Found " ++ show numSpecs ++ " specs"]
    executions <- fmap concat $ forM specs $
        \(specPath, spec) -> specExecutions specPath spec

    -- Create a pool full of executions.
    let numExecutions = length executions
        numJobs       = oJobs options
    envLogger env Message ["Running " ++ show numExecutions ++
        " executions in " ++ show numJobs ++ " jobs"]
    pool <- IORef.newIORef executions

    -- Spawn a worker to report progress
    progress <- Async.async $ forever $ do
        threadDelay $ 10 * 1000 * 1000
        remaining <- length <$> IORef.readIORef pool
        envLogger env Message $ return $
            "Progress: " ++ show (numExecutions - remaining) ++ "/" ++
            show numExecutions ++ "..."

    -- Spawn some workers to run the executions.
    Async.replicateConcurrently_ numJobs $ worker pool (runExecution env)
    Async.cancel progress

    -- Tell the time.
    endTime <- getCurrentTime
    envLogger env Message
        ["Finished in " ++ showDiffTime (endTime `diffUTCTime` startTime)]

    -- Report summary.
    asserts       <- IORef.readIORef (envCountAsserts  env)
    failures      <- IORef.readIORef (envCountFailures env)
    if failures == 0
        then
            envLogger env Message [
                "Ran " ++ show numSpecs ++ " specs, " ++
                show numExecutions ++ " executions, " ++
                show asserts ++ " asserts, all A-OK!"]
        else do
            envLogger env Error [
                "Ran " ++ show numSpecs ++ " specs, " ++
                show numExecutions ++ " executions, " ++
                show asserts ++ " asserts, " ++ show failures ++ " failed."]
            exitFailure

showDiffTime :: NominalDiffTime -> String
showDiffTime dt = printf "%.2fs" (fromRational (toRational dt) :: Double)
