{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Interpreter
    ( InterpreterM
    , Handle
    , newHandle
    , readPackages
    , readPackageRules
    , readAllRules
    , loadModule
    , insertRule

    , loadBundle
    , saveBundle

    , loadModuleOrBundle

    , compilePackages

    , evalExpr
    , evalVar

    , Eval.StepState (..)
    , mkStepState
    , Eval.Step (..)
    , step
    ) where

import qualified Codec.Compression.GZip          as GZip
import           Control.Lens                    (forOf_, ifor_, to, (^.),
                                                  (^..), _2)
import           Control.Lens.TH                 (makeLenses)
import           Control.Monad                   (foldM)
import           Control.Monad.Parachute         (ParachuteT, fatal,
                                                  mapParachuteT)
import           Control.Monad.Reader            (runReader)
import           Control.Monad.Trans             (liftIO)
import qualified Data.Binary                     as Binary
import qualified Data.ByteString.Lazy            as BL
import qualified Data.HashMap.Strict             as HMS
import qualified Data.HashSet.Extended           as HS
import           Data.IORef.Extended             (IORef)
import qualified Data.IORef.Extended             as IORef
import           Data.Maybe                      (fromMaybe)
import qualified Data.Text.IO                    as T
import           Fregot.Compile.Package          (CompiledPackage)
import qualified Fregot.Compile.Package          as Compile
import           Fregot.Error                    (Error, catchIO)
import qualified Fregot.Error                    as Error
import qualified Fregot.Error.Stack              as Stack
import qualified Fregot.Eval                     as Eval
import qualified Fregot.Eval.Builtins            as Builtins
import qualified Fregot.Eval.Cache               as Cache
import           Fregot.Eval.Monad               (EvalCache)
import           Fregot.Eval.Value               (emptyObject)
import           Fregot.Interpreter.Bundle
import qualified Fregot.Interpreter.Dependencies as Deps
import           Fregot.Names
import qualified Fregot.Names.Renamer            as Renamer
import qualified Fregot.Parser                   as Parser
import qualified Fregot.Prepare                  as Prepare
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
    { _sources      :: !Sources.Handle
    -- | List of modules, and files we loaded them from.  Grouped by package
    -- name.
    , _modules      :: !(IORef (HMS.HashMap PackageName ModuleBatch))
    -- | Map of compiled packages.  Dynamically generated from the modules.
    , _compiled     :: !(IORef (HMS.HashMap PackageName CompiledPackage))
    , _cache        :: !EvalCache
    , _cacheVersion :: !Cache.Version
    }

$(makeLenses ''Handle)

newHandle
    :: Sources.Handle
    -> IO Handle
newHandle _sources = do
    _modules      <- liftIO $ IORef.newIORef HMS.empty
    _compiled     <- liftIO $ IORef.newIORef HMS.empty
    _cache        <- liftIO $ Cache.new
    _cacheVersion <- liftIO $ Cache.bump _cache
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

loadModule :: Handle -> FilePath -> InterpreterM ()
loadModule h path = do
    -- Read the source code and parse the module.
    input <- catchIO $ T.readFile path
    liftIO $ IORef.atomicModifyIORef_ (h ^. sources) $
        Sources.insert sourcep input
    modul <- Parser.lexAndParse Parser.parseModule sourcep input

    -- Insert or replace the module.
    insertModule h sourcep modul
  where
    sourcep = Sources.FileInput path

loadBundle :: Handle -> FilePath -> InterpreterM ()
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

loadModuleOrBundle :: Handle -> FilePath -> InterpreterM ()
loadModuleOrBundle h path = case listExtensions path of
    "rego" : _            -> loadModule h path
    "bundle" : "rego" : _ -> loadBundle h path
    _                     -> fatal $ Error.mkErrorNoMeta "interpreter" $
        "Unknown rego file extension:" <+> PP.pretty path <+>
        ", expected .rego or .bundle.rego"

saveBundle :: Handle -> FilePath -> InterpreterM ()
saveBundle h path = liftIO $ do
    _bundleSources <- IORef.readIORef (h ^. sources)
    _bundleModules <- IORef.readIORef (h ^. modules)
    BL.writeFile path $ GZip.compress $ Binary.encode $ Bundle {..}

-- | Get a single package by package name.  If the package does not exist, an
-- empty one is created.
--
-- TODO(jaspervdj): We should probably crash rather than creating an empty
-- package.
readPreparedPackage :: Handle -> PackageName -> InterpreterM PreparedPackage
readPreparedPackage h pkgname = do
    modmap <- liftIO $ IORef.readIORef (h ^. modules)
    let mods = maybe [] (map snd) (HMS.lookup pkgname modmap)
        pkg0 = Prepare.empty pkgname
        pkgRules = HS.toHashSetOf
            (traverse . Sugar.modulePolicy . traverse . Sugar.ruleHead . Sugar.ruleName)
            mods
    foldM (addMod pkgRules) pkg0 mods
  where
    addMod pkgRules pkg0 modul0 = do
        -- TODO(jaspervdj): Clean up this pattern
        imports <- Prepare.prepareImports (modul0 ^. Sugar.moduleImports)
        let renamerEnv = Renamer.RenamerEnv
                Builtins.builtins imports pkgname pkgRules
        modul1 <- mapParachuteT
            (return . flip runReader renamerEnv)
            (Renamer.renameModule modul0)

        foldM
            (\pkg rule -> Prepare.insert imports rule pkg)
            pkg0
            (modul1 ^. Sugar.modulePolicy)

-- | Compile a specific package.  This will compile its dependencies first.
readCompiledPackage
    :: Handle -> PackageName -> InterpreterM CompiledPackage
readCompiledPackage h want = do
    graph <- liftIO (readDependencyGraph h)
    comp0 <- liftIO $ IORef.readIORef (h ^. compiled)
    plan  <- case Deps.plan graph (HS.singleton want) of
        Left _  -> fail "todo: dependency planning error"
        Right x -> return x

    -- Execute plan.
    comp1 <- case plan of
        []    -> return comp0
        _ : _ -> do
            comp1 <- foldM
                (\uni pkgname -> do
                    prep <- readPreparedPackage h pkgname
                    cp   <- Compile.compilePackage uni prep
                    return $ HMS.insert pkgname cp uni)
                comp0
                plan
            liftIO $ IORef.writeIORef (h ^. compiled) comp1
            return comp1

    case HMS.lookup want comp1 of
        Just cp -> return cp
        Nothing -> fail $ "package not found: " ++ show want

compilePackages :: Handle -> InterpreterM ()
compilePackages h = do
    mods  <- liftIO $ IORef.readIORef (h ^. modules)
    comps <- liftIO $ IORef.readIORef (h ^. compiled)
    let needComp = mods `HMS.difference` comps
    ifor_ needComp $ \pkgname _ -> readCompiledPackage h pkgname

-- | Get a list of loaded packages.
readPackages :: Handle -> InterpreterM [PackageName]
readPackages h = do
    pkgs <- liftIO $ IORef.readIORef (h ^. compiled)
    return (HMS.keys pkgs)

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

eval
    :: Handle -> Eval.Context -> PackageName -> Eval.EvalM a
    -> InterpreterM (Eval.Document a)
eval h ctx pkgname mx = do
    comp <- liftIO $ IORef.readIORef (h ^. compiled)
    pkg  <- readCompiledPackage h pkgname
    let env = Eval.Environment
            { Eval._packages     = comp
            , Eval._package      = pkg
            , Eval._inputDoc     = emptyObject
            , Eval._imports      = mempty
            , Eval._cache        = h ^. cache
            , Eval._cacheVersion = h ^. cacheVersion
            , Eval._stack        = Stack.empty
            }

    either fatal return =<< liftIO (Eval.runEvalM env ctx mx)

evalExpr
    :: Handle -> Eval.Context -> PackageName -> Sugar.Expr SourceSpan Var
    -> InterpreterM (Eval.Document Eval.Value)
evalExpr h ctx pkgname expr = do
    pkg   <- readCompiledPackage h pkgname

    let renamerEnv = Renamer.RenamerEnv
            Builtins.builtins
            mempty  -- No imports?
            pkgname
            (HS.fromList $ Compile.rules pkg)
    rterm <- mapParachuteT
        (return . flip runReader renamerEnv)
        (Renamer.renameExpr expr)

    pterm <- Prepare.prepareExpr rterm
    cterm <- Compile.compileTerm pkg safeLocals pterm
    eval h ctx pkgname (Eval.evalTerm cterm)
  where
    safeLocals = Compile.Safe $ HS.fromList $ ctx ^. Eval.locals . to HMS.keys

evalVar
    :: Handle -> SourceSpan -> PackageName -> Var
    -> InterpreterM (Eval.Document Eval.Value)
evalVar h source pkgname var =
    let expr = Sugar.TermE source (Sugar.VarT source var) in
    evalExpr h Eval.emptyContext pkgname expr

mkStepState
    :: Handle -> PackageName -> Sugar.Expr SourceSpan Var
    -> InterpreterM (Eval.StepState Eval.Value)
mkStepState h pkgname expr = do
    comp  <- liftIO $ IORef.readIORef (h ^. compiled)
    pkg   <- readCompiledPackage h pkgname

    let renamerEnv = Renamer.RenamerEnv
            Builtins.builtins
            mempty  -- No imports?
            pkgname
            (HS.fromList $ Compile.rules pkg)
    rexpr <- mapParachuteT
        (return . flip runReader renamerEnv)
        (Renamer.renameExpr expr)

    pterm <- Prepare.prepareExpr rexpr
    cterm <- Compile.compileTerm pkg mempty pterm
    let env = Eval.Environment
            { Eval._packages     = comp
            , Eval._package      = pkg
            , Eval._inputDoc     = emptyObject
            , Eval._imports      = mempty
            , Eval._cache        = h ^. cache
            , Eval._cacheVersion = h ^. cacheVersion
            , Eval._stack        = Stack.empty
            }
    return $ Eval.mkStepState env (Eval.evalTerm cterm)

step
    :: Handle -> Eval.StepState Eval.Value
    -> InterpreterM (Eval.Step Eval.Value)
step _ = liftIO . Eval.stepEvalM
