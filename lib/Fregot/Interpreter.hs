{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Interpreter
    ( InterpreterM
    , Handle
    , newHandle

    , readBuiltins
    , insertBuiltin

    , readPackages
    , readPackage
    , readPackageRules

    , readAllRules
    , loadModule
    , insertRule

    , loadBundle
    , saveBundle

    , loadModuleOrBundle

    , compilePackages

    , Eval.EnvContext (..), Eval.ecEnvironment, Eval.ecContext
    , evalExpr
    , evalVar

    , Eval.ResumeStep
    , newResumeStep
    , Eval.Step (..)
    , step

    , setInput
    , setInputFile
    ) where

import qualified Codec.Compression.GZip          as GZip
import           Control.Lens                    (forOf_, ifor_, to, (^.),
                                                  (^..), _2)
import           Control.Lens.TH                 (makeLenses)
import           Control.Monad                   (foldM)
import           Control.Monad.Identity          (Identity (..))
import           Control.Monad.Parachute
import           Control.Monad.Reader            (runReader)
import           Control.Monad.Trans             (liftIO)
import qualified Data.Aeson                      as Aeson
import qualified Data.Binary                     as Binary
import qualified Data.ByteString.Lazy            as BL
import qualified Data.HashMap.Strict             as HMS
import qualified Data.HashSet.Extended           as HS
import           Data.IORef.Extended             (IORef)
import qualified Data.IORef.Extended             as IORef
import           Data.Maybe                      (fromMaybe)
import qualified Data.Text.IO                    as T
import           Data.Traversable.HigherOrder    (htraverse)
import           Fregot.Compile.Package          (CompiledPackage)
import qualified Fregot.Compile.Package          as Compile
import           Fregot.Error                    (Error, catchIO)
import qualified Fregot.Error                    as Error
import qualified Fregot.Error.Stack              as Stack
import qualified Fregot.Eval                     as Eval
import           Fregot.Eval.Builtins            (Builtin, Builtins,
                                                  defaultBuiltins)
import qualified Fregot.Eval.Cache               as Cache
import qualified Fregot.Eval.Json                as Eval.Json
import           Fregot.Eval.Monad               (EvalCache)
import           Fregot.Eval.Value               (emptyObject)
import           Fregot.Interpreter.Bundle
import qualified Fregot.Interpreter.Dependencies as Deps
import           Fregot.Names
import qualified Fregot.Names.Renamer            as Renamer
import qualified Fregot.Parser                   as Parser
import qualified Fregot.Prepare                  as Prepare
import           Fregot.Prepare.Ast              (Function (..))
import           Fregot.Prepare.Package          (PreparedPackage)
import qualified Fregot.Prepare.Package          as Prepare
import           Fregot.PrettyPrint              ((<$$>), (<+>))
import qualified Fregot.PrettyPrint              as PP
import           Fregot.Sources                  (SourcePointer)
import qualified Fregot.Sources                  as Sources
import           Fregot.Sources.SourceSpan       (SourceSpan)
import qualified Fregot.Sugar                    as Sugar
import           System.FilePath.Extended        (listExtensions)

type InterpreterM a = ParachuteT Error IO a

data Handle = Handle
    { _builtins :: !(IORef (Builtins Identity))
    , _sources  :: !Sources.Handle
    -- | List of modules, and files we loaded them from.  Grouped by package
    -- name.
    , _modules  :: !(IORef (HMS.HashMap PackageName ModuleBatch))
    -- | Map of compiled packages.  Dynamically generated from the modules.
    , _compiled :: !(IORef (HMS.HashMap PackageName CompiledPackage))
    , _cache    :: !(IORef EvalCache)
    , _inputDoc :: !(IORef Eval.Value)
    }

$(makeLenses ''Handle)

newHandle
    :: Sources.Handle
    -> IO Handle
newHandle _sources = do
    initializedBuiltins <- traverse (htraverse (fmap Identity)) defaultBuiltins

    _builtins     <- IORef.newIORef initializedBuiltins
    _modules      <- IORef.newIORef HMS.empty
    _compiled     <- IORef.newIORef HMS.empty
    _cache        <- Cache.new >>= IORef.newIORef
    _inputDoc     <- IORef.newIORef emptyObject
    return Handle {..}

readDependencyGraph
    :: Handle -> IO (Deps.Graph PackageName)
readDependencyGraph h = do
    done <- IORef.readIORef (h ^. compiled)
    mods <- IORef.readIORef (h ^. modules)
    let dependencies k = maybe [] (^.. deps) (HMS.lookup k mods)
    return $ Deps.Graph done dependencies
  where
    deps = traverse . _2 . Sugar.moduleImports . traverse . Sugar.importPackage

-- | Auxiliary function for hooking into the renamer.
runRenamerT :: Renamer.RenamerEnv -> Renamer.RenamerM a -> InterpreterM a
runRenamerT renv = mapParachuteT (return . flip runReader renv)

insertModule
    :: Handle -> SourcePointer -> Sugar.Module SourceSpan Var -> InterpreterM ()
insertModule h sourcep modul = do
    -- Insert or replace the module.
    let pkgname = modul ^. Sugar.modulePackage
    liftIO $ IORef.atomicModifyIORef_ (h ^. modules) $ \mods ->
        let m1 = fromMaybe [] (HMS.lookup pkgname mods)
            m2 = (sourcep, modul) : filter ((/= sourcep) . fst) m1 in
        HMS.insert pkgname m2 mods

    -- Compute all modules that depend on the module, so we can evict all of
    -- them.
    depGraph <- liftIO (readDependencyGraph h)
    let evict = Deps.evict depGraph (HS.singleton pkgname)

    -- Remove everything that's in the evict set.
    liftIO $ IORef.atomicModifyIORef_ (h ^. compiled) $
        HMS.filterWithKey (\k _ -> not (k `HS.member` evict))

    dieIfErrors

loadModule
    :: Handle -> Parser.ParserOptions -> FilePath -> InterpreterM PackageName
loadModule h popts path = do
    -- Read the source code and parse the module.
    input <- catchIO $ T.readFile path
    liftIO $ IORef.atomicModifyIORef_ (h ^. sources) $
        Sources.insert sourcep input
    modul <- Parser.lexAndParse (Parser.parseModule popts) sourcep input

    -- Insert or replace the module.
    insertModule h sourcep modul
    return $ modul ^. Sugar.modulePackage
  where
    sourcep = Sources.FileInput path

loadBundle :: Handle -> FilePath -> InterpreterM [PackageName]
loadBundle h path = do
    errOrBundle <- Binary.decodeOrFail . GZip.decompress <$>
        liftIO (BL.readFile path)
    case errOrBundle of
        Left (_, _, err) -> fatal $ Error.mkErrorNoMeta "interpreter" $
            "Loading bundle" <+> PP.pretty path <+> "failed:" <$$>
            PP.ind (PP.pretty err)
        Right (_, _, bundle) -> do
            liftIO $ IORef.atomicModifyIORef_ (h ^. sources)
                (mappend (bundle ^. bundleSources))
            forOf_ (bundleModules . traverse . traverse) bundle $
                \(sourcep, modul) -> insertModule h sourcep modul
            return $ HMS.keys $ bundle ^. bundleModules

loadModuleOrBundle
    :: Handle -> Parser.ParserOptions -> FilePath -> InterpreterM [PackageName]
loadModuleOrBundle h popts path = case listExtensions path of
    "rego" : _            -> pure <$> loadModule h popts path
    "bundle" : "rego" : _ -> loadBundle h path
    _                     -> fatal $ Error.mkErrorNoMeta "interpreter" $
        "Unknown rego file extension:" <+> PP.pretty path <+>
        ", expected .rego or .bundle.rego"

saveBundle :: Handle -> FilePath -> InterpreterM ()
saveBundle h path = liftIO $ do
    _bundleSources <- IORef.readIORef (h ^. sources)
    _bundleModules <- IORef.readIORef (h ^. modules)
    BL.writeFile path $ GZip.compress $ Binary.encode $ Bundle {..}

-- TODO(jaspervdj): We should probably crash rather than creating an empty
-- package.
preparePackage
    :: Builtins Identity
    -> HMS.HashMap PackageName ModuleBatch
    -> HMS.HashMap PackageName CompiledPackage
    -> PackageName
    -> InterpreterM PreparedPackage
preparePackage builtin modmap dependencies pkgname = do
    let mods = maybe [] (map snd) (HMS.lookup pkgname modmap)
        pkg0 = Prepare.empty pkgname
        pkgRules = HS.toHashSetOf
            (traverse . Sugar.modulePolicy . traverse . Sugar.ruleHead . Sugar.ruleName)
            mods
    foldM (addMod pkgRules) pkg0 mods
  where
    addMod pkgRules pkg0 modul0 = do
        -- Rename module.
        imports <- Prepare.prepareImports (modul0 ^. Sugar.moduleImports)
        let renamerEnv = Renamer.RenamerEnv
                builtin imports pkgname pkgRules dependencies
        modul1 <- runRenamerT renamerEnv $ Renamer.renameModule modul0

        foldM
            (\pkg rule -> Prepare.insert imports rule pkg)
            pkg0
            (modul1 ^. Sugar.modulePolicy)

-- | Compile a specific package.  This will compile its dependencies first.
readCompiledPackage
    :: Handle -> PackageName -> InterpreterM CompiledPackage
readCompiledPackage h want = do
    graph   <- liftIO (readDependencyGraph h)
    modmap  <- liftIO $ IORef.readIORef (h ^. modules)
    comp0   <- liftIO $ IORef.readIORef (h ^. compiled)
    builtin <- liftIO $ IORef.readIORef (h ^. builtins)
    plan    <- case Deps.plan graph (HS.singleton want) of
        Right x -> return x
        Left depError -> fatal $
            Error.mkErrorNoMeta "interpreter" (PP.pretty depError)

    -- Execute plan.
    comp1 <- case plan of
        []    -> return comp0
        _ : _ -> do
            comp1 <- foldM
                (\uni pkgname -> do
                    prep <- preparePackage builtin modmap uni pkgname
                    cp   <- Compile.compilePackage builtin uni prep
                    dieIfErrors
                    return $ HMS.insert pkgname cp uni)
                comp0
                plan
            liftIO $ IORef.writeIORef (h ^. compiled) comp1
            return comp1

    case HMS.lookup want comp1 of
        Just cp -> return cp
        Nothing -> fatal $ Error.mkErrorNoMeta "interpreter"
            "Internal error: package not found after compilation"

compilePackages :: Handle -> InterpreterM ()
compilePackages h = do
    mods  <- liftIO $ IORef.readIORef (h ^. modules)
    comps <- liftIO $ IORef.readIORef (h ^. compiled)
    let needComp = mods `HMS.difference` comps
    ifor_ needComp $ \pkgname _ -> readCompiledPackage h pkgname

-- | Get a list of all builtins.
readBuiltins :: Handle -> InterpreterM [Function]
readBuiltins h = liftIO $ HMS.keys <$> IORef.readIORef (h ^. builtins)

-- | Register a builtin.
insertBuiltin :: Handle -> Function -> Builtin Identity -> InterpreterM ()
insertBuiltin h k b =
    liftIO $ IORef.atomicModifyIORef_ (h ^. builtins) $ HMS.insert k b

-- | Get a list of loaded packages.
readPackages :: Handle -> InterpreterM [PackageName]
readPackages h = liftIO $ HMS.keys <$> IORef.readIORef (h ^. compiled)

-- | Obtain a compiled package.
readPackage :: Handle -> PackageName -> InterpreterM CompiledPackage
readPackage = readCompiledPackage

-- | Read all rules in a specific package.
readPackageRules :: Handle -> PackageName -> InterpreterM [Var]
readPackageRules h pkgname = do
    pkgs <- liftIO $ IORef.readIORef (h ^. compiled)
    return $ case HMS.lookup pkgname pkgs of
        Nothing  -> []
        Just pkg -> Prepare.rules pkg

-- | Read all available rules.  This is used to enumerate all rules starting
-- with `test_` by the tester.
readAllRules :: Handle -> InterpreterM [(PackageName, Var)]
readAllRules h = do
    pkgs <- liftIO $ IORef.readIORef (h ^. compiled)
    return $ do
        (pkgname, pkg) <- HMS.toList pkgs
        rule           <- Prepare.rules pkg
        return (pkgname, rule)

insertRule
    :: Handle -> PackageName -> SourcePointer -> Sugar.Rule SourceSpan Var
    -> InterpreterM ()
insertRule h pkgname sourcep rule =
    insertModule h sourcep modul
  where
    modul = Sugar.Module
        { _modulePackage = pkgname
        , _moduleImports = []  -- TODO(jaspervdj): REPL imports here?
        , _modulePolicy  = [rule]
        }

newEvalContext :: Handle -> InterpreterM Eval.EnvContext
newEvalContext h = Eval.EnvContext
    <$> pure Eval.emptyContext
    <*> (Eval.Environment
        <$> liftIO (IORef.readIORef (h ^. builtins))
        <*> liftIO (IORef.readIORef (h ^. compiled))
        <*> liftIO (IORef.readIORef (h ^. inputDoc))
        <*> liftIO (IORef.readIORef (h ^. cache))
        <*> pure Stack.empty)

eval
    :: Handle -> Maybe Eval.EnvContext -> PackageName -> Eval.EvalM a
    -> InterpreterM (Eval.Document a)
eval h mbEvalEnvCtx _pkgname mx = do
    envctx <- maybe (newEvalContext h) return mbEvalEnvCtx
    either fatal return =<< liftIO (Eval.runEvalM envctx mx)

evalExpr
    :: Handle -> Maybe Eval.EnvContext -> PackageName
    -> Sugar.Expr SourceSpan Var -> InterpreterM (Eval.Document Eval.Value)
evalExpr h mbEvalEnvCtx pkgname expr = do
    comp    <- liftIO $ IORef.readIORef (h ^. compiled)
    pkg     <- readCompiledPackage h pkgname
    builtin <- liftIO $ IORef.readIORef (h ^. builtins)

    -- Rename expression.
    let renamerEnv = Renamer.RenamerEnv
            builtin
            mempty  -- No imports?
            pkgname
            (HS.fromList $ Compile.rules pkg)
            comp
    rterm <- runRenamerT renamerEnv $ Renamer.renameExpr expr

    pterm <- Prepare.prepareExpr rterm
    cterm <- Compile.compileTerm builtin pkg safeLocals pterm
    dieIfErrors
    eval h mbEvalEnvCtx pkgname (Eval.evalTerm cterm)
  where
    safeLocals = Compile.Safe $ HS.fromList $ case mbEvalEnvCtx of
        Nothing -> mempty
        Just ec -> ec ^. Eval.ecContext . Eval.locals . to HMS.keys

evalVar
    :: Handle -> Maybe Eval.EnvContext -> SourceSpan -> PackageName -> Var
    -> InterpreterM (Eval.Document Eval.Value)
evalVar h mbEvalEnvCtx source pkgname var = do
    let expr = Sugar.TermE source (Sugar.VarT source var)
    evalExpr h mbEvalEnvCtx pkgname expr

newResumeStep
    :: Handle -> PackageName -> Sugar.Expr SourceSpan Var
    -> InterpreterM (Eval.ResumeStep Eval.Value)
newResumeStep h pkgname expr = do
    envctx <- newEvalContext h
    pkg    <- readCompiledPackage h pkgname

    -- Rename expression.
    let builtin    = envctx ^. Eval.ecEnvironment . Eval.builtins
        renamerEnv = Renamer.RenamerEnv
            builtin
            mempty  -- No imports?
            pkgname
            (HS.fromList $ Compile.rules pkg)
            (envctx ^. Eval.ecEnvironment . Eval.packages)
    rexpr <- runRenamerT renamerEnv $ Renamer.renameExpr expr

    pterm  <- Prepare.prepareExpr rexpr
    cterm  <- Compile.compileTerm builtin pkg mempty pterm

    dieIfErrors
    return $ Eval.newResumeStep envctx (Eval.evalTerm cterm)

step
    :: Handle -> Eval.ResumeStep Eval.Value
    -> InterpreterM (Eval.Step Eval.Value)
step _ = liftIO . Eval.runStep

setInput :: Handle -> Aeson.Value -> InterpreterM ()
setInput h val = liftIO $ do
    IORef.readIORef (h ^. cache) >>=
        Cache.bump >>=
        IORef.writeIORef (h ^. cache)
    IORef.writeIORef (h ^. inputDoc) (Eval.Json.toValue val)

setInputFile :: Handle -> FilePath -> InterpreterM ()
setInputFile h path = do
    errOrVal <- liftIO $ Aeson.eitherDecodeFileStrict' path
    val      <- case errOrVal of
        Right v  -> pure (v :: Aeson.Value)
        Left err -> fatal $ Error.mkErrorNoMeta "interpreter" $
            "Loading input file" <+> PP.pretty path <+> "failed:" <$$>
            PP.ind (PP.pretty err)
    setInput h val
