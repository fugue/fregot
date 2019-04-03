{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Fregot.Interpreter
    ( InterpreterM
    , Handle
    , newHandle
    , readPackageRules
    , readAllRules
    , loadModule
    , insertRule

    , compilePackages

    , evalExpr
    , evalVar
    ) where

import           Control.Lens              (ifor, (^.))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad             (foldM)
import           Control.Monad.Parachute   (ParachuteT, fatal)
import           Control.Monad.Trans       (liftIO)
import qualified Data.HashMap.Strict       as HMS
import           Data.IORef.Extended       (IORef)
import qualified Data.IORef.Extended       as IORef
import           Data.Maybe                (fromMaybe)
import qualified Data.Text.IO              as T
import           Fregot.Compile.Package    (CompiledPackage)
import qualified Fregot.Compile.Package    as Compile
import           Fregot.Error              (Error, catchIO)
import qualified Fregot.Eval               as Eval
import           Fregot.Eval.Cache         (Cache)
import qualified Fregot.Eval.Cache         as Cache
import           Fregot.Eval.Value         (emptyObject)
import           Fregot.Eval.Value         (Value)
import qualified Fregot.Parser             as Parser
import qualified Fregot.Prepare            as Prepare
import           Fregot.Prepare.Package    (PreparedPackage)
import qualified Fregot.Prepare.Package    as Prepare
import           Fregot.Sources            (SourcePointer)
import qualified Fregot.Sources            as Sources
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Fregot.Sugar              (PackageName, Var)
import qualified Fregot.Sugar              as Sugar

type InterpreterM a = ParachuteT Error IO a

-- | The modules that make up a package.
type ModuleBatch = [(SourcePointer, Sugar.Module SourceSpan)]

data Handle = Handle
    { _sources      :: !Sources.Handle
    -- | List of modules, and files we loaded them from.  Grouped by package
    -- name.
    , _modules      :: !(IORef (HMS.HashMap PackageName ModuleBatch))
    -- | Map of compiled packages.  Dynamically generated from the modules.
    , _compiled     :: !(IORef (HMS.HashMap PackageName CompiledPackage))
    , _cache        :: !(Cache (PackageName, Var) [Value])
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
    :: Handle -> PackageName -> Eval.EvalM a
    -> InterpreterM (Eval.Document a)
eval h pkgname mx = do
    comp <- liftIO $ IORef.readIORef (h ^. compiled)
    pkg  <- readCompiledPackage h pkgname
    let env = Eval.Environment
            { Eval._packages     = comp
            , Eval._package      = pkg
            , Eval._inputDoc     = emptyObject
            , Eval._imports      = mempty
            , Eval._cache        = h ^. cache
            , Eval._cacheVersion = h ^. cacheVersion
            }

    either fatal return =<< liftIO (Eval.runEvalM env mx)

evalExpr
    :: Handle -> PackageName -> Sugar.Expr SourceSpan
    -> InterpreterM (Eval.Document Eval.Value)
evalExpr h pkgname expr = do
    pkg   <- readCompiledPackage h pkgname
    pterm <- Prepare.prepareExpr expr
    cterm <- Compile.compileTerm pkg pterm
    eval h pkgname (Eval.evalTerm cterm)

evalVar
    :: Handle -> SourceSpan -> PackageName -> Var
    -> InterpreterM (Eval.Document Eval.Value)
evalVar h source pkgname = eval h pkgname . Eval.evalVar source
