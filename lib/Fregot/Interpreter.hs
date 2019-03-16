{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Fregot.Interpreter
    ( InterpreterM
    , Handle
    , newHandle
    , readRules
    , loadModule
    , insertRule

    , compilePackages

    , evalExpr
    , evalVar
    ) where

import           Control.Lens              ((^.))
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
import           Fregot.Error              (Error)
import qualified Fregot.Eval               as Eval
import           Fregot.Eval.Value         (emptyObject)
import qualified Fregot.Parser             as Parser
import qualified Fregot.Prepare            as Prepare
import           Fregot.Prepare.Package    (PreparedPackage)
import qualified Fregot.Prepare.Package    as Prepare
import qualified Fregot.Sources            as Sources
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Fregot.Sugar              (PackageName, Var)
import qualified Fregot.Sugar              as Sugar

type InterpreterM a = ParachuteT Error IO a

data Handle = Handle
    { _sources  :: !Sources.Handle
    , _packages :: !(IORef (HMS.HashMap PackageName PreparedPackage))
    , _compiled :: !(IORef (HMS.HashMap PackageName CompiledPackage))
    }

$(makeLenses ''Handle)

newHandle
    :: Sources.Handle
    -> IO Handle
newHandle _sources = do
    _packages <- liftIO $ IORef.newIORef HMS.empty
    _compiled <- liftIO $ IORef.newIORef HMS.empty
    return Handle {..}

loadModule :: Handle -> FilePath -> InterpreterM ()
loadModule h path = do
    -- Read the source code and parse the module.
    input <- liftIO $ T.readFile path
    liftIO $ IORef.atomicModifyIORef_ (h ^. sources) $
        Sources.insert sourcep input
    modul <- Parser.lexAndParse Parser.parseModule sourcep input

    -- Insert the module into the packages system.
    insertModule h modul
  where
    sourcep = Sources.FileInput path

-- | Get a single package by package name.  If the package does not exist, an
-- empty one is created.
readPackage :: Handle -> PackageName -> InterpreterM PreparedPackage
readPackage h pkgname =
    fromMaybe (Prepare.empty pkgname) . HMS.lookup pkgname <$>
    liftIO (IORef.readIORef (h ^. packages))

-- | Get a compiled package.  If it does not exist, it is compiled.
readCompiledPackage :: Handle -> PackageName -> InterpreterM CompiledPackage
readCompiledPackage h pkgname = do
    comp <- liftIO $ IORef.readIORef (h ^. compiled)
    case HMS.lookup pkgname comp of
        Just cp -> return cp
        Nothing -> do
            prep <- readPackage h pkgname
            cp   <- Compile.compile prep
            liftIO $ IORef.atomicModifyIORef_ (h ^. compiled) $
                HMS.insert pkgname cp
            return cp

-- | Read all available rules.  This is used to enumerate all rules starting
-- with `test_` by the tester.
readRules :: Handle -> InterpreterM [(PackageName, Var)]
readRules h = do
    pkgs <- liftIO $ IORef.readIORef (h ^. packages)
    return $ do
        (pkgname, pkg) <- HMS.toList pkgs
        rule           <- Prepare.rules pkg
        return (pkgname, rule)

-- | TODO(jaspervdj): This will require a lock if we concurrently load modules.
insertModule :: Handle -> Sugar.Module SourceSpan -> InterpreterM ()
insertModule h modul = do
    -- Lookup an existing package or create a new one if necessary.
    package0 <- readPackage h pkgname

    -- One by one, add the rules in the sugared module to the package.
    imports <- Prepare.prepareImports (modul ^. Sugar.moduleImports)
    package1 <- foldM
        (\pkg rule -> Prepare.insert imports rule pkg)
        package0
        (modul ^. Sugar.modulePolicy)

    -- Save the modified package.
    liftIO $ IORef.atomicModifyIORef_ (h ^. packages) $
        HMS.insert pkgname package1

    -- Remove the corresponding compiled module.
    liftIO $ IORef.atomicModifyIORef_ (h ^. compiled) $ HMS.delete pkgname
  where
    pkgname = modul ^. Sugar.modulePackage

insertRule
    :: Handle -> PackageName -> Sugar.Rule SourceSpan -> InterpreterM ()
insertRule h pkgname rule = do
    package0 <- readPackage h pkgname
    package1 <- Prepare.insert mempty rule package0
    liftIO $ IORef.atomicModifyIORef_ (h ^. packages) $
        HMS.insert pkgname package1

    -- Remove the corresponding compiled module.
    liftIO $ IORef.atomicModifyIORef_ (h ^. compiled) $ HMS.delete pkgname

compilePackages :: Handle -> InterpreterM ()
compilePackages h = do
    preps <- liftIO $ IORef.readIORef (h ^. packages)
    comps <- liftIO $ IORef.readIORef (h ^. compiled)
    let needComp = preps `HMS.difference` comps
    newComp <- traverse Compile.compile needComp
    liftIO $ IORef.atomicModifyIORef_ (h ^. compiled) $
        \oldComp -> oldComp <> newComp

eval
    :: Handle -> PackageName -> Eval.EvalM a
    -> InterpreterM (Eval.Document a)
eval h pkgname mx = do
    comp <- liftIO $ IORef.readIORef (h ^. compiled)
    pkg  <- readCompiledPackage h pkgname
    let env = Eval.Environment comp pkg emptyObject mempty
    either fatal return =<< liftIO (Eval.runEvalM env mx)

evalExpr
    :: Handle -> PackageName -> Sugar.Expr SourceSpan
    -> InterpreterM (Eval.Document Eval.Value)
evalExpr h pkgname expr = do
    term <- Prepare.prepareExpr expr
    -- TODO(jaserpvdj): Compile.compileTerm ?
    eval h pkgname (Eval.evalTerm term)

evalVar
    :: Handle -> SourceSpan -> PackageName -> Var
    -> InterpreterM (Eval.Document Eval.Value)
evalVar h source pkgname = eval h pkgname . Eval.evalVar source
