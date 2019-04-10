{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Interpreter
    ( InterpreterM
    , Handle
    , newHandle
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

import qualified Codec.Compression.GZip    as GZip
import           Control.Lens              (forOf_, ifor, to, (^.))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad             (foldM)
import           Control.Monad.Parachute   (ParachuteT, fatal)
import           Control.Monad.Trans       (liftIO)
import qualified Data.Binary               as Binary
import qualified Data.ByteString.Lazy      as BL
import qualified Data.HashMap.Strict       as HMS
import qualified Data.HashSet.Extended     as HS
import           Data.IORef.Extended       (IORef)
import qualified Data.IORef.Extended       as IORef
import           Data.Maybe                (fromMaybe)
import qualified Data.Text.IO              as T
import           Fregot.Compile.Package    (CompiledPackage)
import qualified Fregot.Compile.Package    as Compile
import           Fregot.Error              (Error, catchIO)
import qualified Fregot.Error              as Error
import qualified Fregot.Eval               as Eval
import           Fregot.Eval.Value         (emptyObject)
import           Fregot.Interpreter.Bundle
import qualified Fregot.Parser             as Parser
import qualified Fregot.Prepare            as Prepare
import           Fregot.Prepare.Package    (PreparedPackage)
import qualified Fregot.Prepare.Package    as Prepare
import           Fregot.PrettyPrint        ((<$$>), (<+>))
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources            (SourcePointer)
import qualified Fregot.Sources            as Sources
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Fregot.Sugar              (PackageName, Var)
import qualified Fregot.Sugar              as Sugar
import           System.FilePath           (takeExtension)

type InterpreterM a = ParachuteT Error IO a

data Handle = Handle
    { _sources  :: !Sources.Handle
    -- | List of modules, and files we loaded them from.  Grouped by package
    -- name.
    , _modules  :: !(IORef (HMS.HashMap PackageName ModuleBatch))
    -- | Map of compiled packages.  Dynamically generated from the modules.
    , _compiled :: !(IORef (HMS.HashMap PackageName CompiledPackage))
    }

$(makeLenses ''Handle)

newHandle
    :: Sources.Handle
    -> IO Handle
newHandle _sources = do
    _modules  <- liftIO $ IORef.newIORef HMS.empty
    _compiled <- liftIO $ IORef.newIORef HMS.empty
    return Handle {..}

insertModule
    :: Handle -> SourcePointer -> Sugar.Module SourceSpan -> InterpreterM ()
insertModule h sourcep modul = do
    -- Insert or replace the module.
    let pkgname = modul ^. Sugar.modulePackage
    liftIO $ IORef.atomicModifyIORef_ (h ^. modules) $ \mods ->
        let m1 = fromMaybe [] (HMS.lookup pkgname mods)
            m2 = (sourcep, modul) : filter ((/= sourcep) . fst) m1 in
        HMS.insert pkgname m2 mods

    -- Remove the corresponding compiled module.
    liftIO $ IORef.atomicModifyIORef_ (h ^. compiled) $ HMS.delete pkgname

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
loadModuleOrBundle h path = case takeExtension path of
    ".rego"  -> loadModule h path
    ".regob" -> loadBundle h path
    ext      -> fatal $ Error.mkErrorNoMeta "interpreter" $
        "Unknown rego file extension:" <+> PP.pretty ext

saveBundle :: Handle -> FilePath -> InterpreterM ()
saveBundle h path = liftIO $ do
    _bundleSources <- IORef.readIORef (h ^. sources)
    _bundleModules <- IORef.readIORef (h ^. modules)
    BL.writeFile path $ GZip.compress $ Binary.encode $ Bundle {..}

-- | Get a single package by package name.  If the package does not exist, an
-- empty one is created.
readPreparedPackage :: Handle -> PackageName -> InterpreterM PreparedPackage
readPreparedPackage h pkgname = do
    modmap <- liftIO $ IORef.readIORef (h ^. modules)
    let mods = maybe [] (map snd) (HMS.lookup pkgname modmap)
        pkg0 = Prepare.empty pkgname
    foldM addMod pkg0 mods

  where
    addMod pkg0 modul = do
        imports <- Prepare.prepareImports (modul ^. Sugar.moduleImports)
        foldM
            (\pkg rule -> Prepare.insert imports rule pkg)
            pkg0
            (modul ^. Sugar.modulePolicy)

-- | Get a compiled package.  If it does not exist, it is compiled.
readCompiledPackage :: Handle -> PackageName -> InterpreterM CompiledPackage
readCompiledPackage h pkgname = do
    comp <- liftIO $ IORef.readIORef (h ^. compiled)
    case HMS.lookup pkgname comp of
        Just cp -> return cp
        Nothing -> do
            prep <- readPreparedPackage h pkgname
            cp   <- Compile.compilePackage prep
            liftIO $ IORef.atomicModifyIORef_ (h ^. compiled) $
                HMS.insert pkgname cp
            return cp

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
    :: Handle -> PackageName -> SourcePointer -> Sugar.Rule SourceSpan
    -> InterpreterM ()
insertRule h pkgname sourcep rule =
    insertModule h sourcep modul
  where
    modul = Sugar.Module
        { _modulePackage = pkgname
        , _moduleImports = []  -- TODO(jaspervdj): REPL imports here?
        , _modulePolicy  = [rule]
        }

compilePackages :: Handle -> InterpreterM ()
compilePackages h = do
    mods  <- liftIO $ IORef.readIORef (h ^. modules)
    comps <- liftIO $ IORef.readIORef (h ^. compiled)
    let needComp = mods `HMS.difference` comps
    newComp <- ifor needComp $ \pkgname _ -> do
        prep <- readPreparedPackage h pkgname
        Compile.compilePackage prep
    liftIO $ IORef.atomicModifyIORef_ (h ^. compiled) $
        \oldComp -> oldComp <> newComp

eval
    :: Handle -> Eval.Context -> PackageName -> Eval.EvalM a
    -> InterpreterM (Eval.Document a)
eval h ctx pkgname mx = do
    comp <- liftIO $ IORef.readIORef (h ^. compiled)
    pkg  <- readCompiledPackage h pkgname
    let env = Eval.Environment comp pkg emptyObject mempty
    either fatal return =<< liftIO (Eval.runEvalM env ctx mx)

evalExpr
    :: Handle -> Eval.Context -> PackageName -> Sugar.Expr SourceSpan
    -> InterpreterM (Eval.Document Eval.Value)
evalExpr h ctx pkgname expr = do
    pkg   <- readCompiledPackage h pkgname
    pterm <- Prepare.prepareExpr expr
    cterm <- Compile.compileTerm pkg safeLocals pterm
    eval h ctx pkgname (Eval.evalTerm cterm)
  where
    safeLocals = Compile.Safe $ HS.fromList $ ctx ^. Eval.locals . to HMS.keys

evalVar
    :: Handle -> SourceSpan -> PackageName -> Var
    -> InterpreterM (Eval.Document Eval.Value)
evalVar h source pkgname =
    eval h Eval.emptyContext pkgname . Eval.evalVar source

mkStepState
    :: Handle -> PackageName -> Sugar.Expr SourceSpan
    -> InterpreterM (Eval.StepState Eval.Value)
mkStepState h pkgname expr = do
    comp  <- liftIO $ IORef.readIORef (h ^. compiled)
    pkg   <- readCompiledPackage h pkgname
    pterm <- Prepare.prepareExpr expr
    cterm <- Compile.compileTerm pkg mempty pterm
    let env = Eval.Environment comp pkg emptyObject mempty
    return $ Eval.mkStepState env (Eval.evalTerm cterm)

step
    :: Handle -> Eval.StepState Eval.Value
    -> InterpreterM (Eval.Step Eval.Value)
step _ = liftIO . Eval.stepEvalM
