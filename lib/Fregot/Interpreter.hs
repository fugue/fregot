{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Fregot.Interpreter
    ( InterpreterM
    , Handle
    , newHandle
    , copyHandle

    , readSources

    , readBuiltins
    , insertBuiltin

    , readPackages
    , readPackageRules

    , readAllRules
    , loadModule
    , insertRule

    , loadBundle
    , saveBundle

    , loadFileByExtension

    , compileRules

    , Eval.EnvContext (..), Eval.ecEnvironment, Eval.ecContext
    , evalQuery
    , evalVar

    , Eval.ResumeStep
    , newResumeStep
    , Eval.Step (..)
    , step

    , setInput
    , setInputFile

    , typeExpr
    ) where

import qualified Codec.Compression.GZip          as GZip
import           Control.Comonad                 (Comonad (..))
import           Control.Lens                    (forOf_, ix, over, preview,
                                                  review, to, (^.), (^..), _1)
import           Control.Lens.TH                 (makeLenses)
import           Control.Monad                   (foldM, guard, unless, when)
import           Control.Monad.Identity          (Identity (..))
import           Control.Monad.Parachute
import           Control.Monad.Reader            (runReader)
import           Control.Monad.Trans             (liftIO)
import qualified Data.Aeson                      as Aeson
import           Data.Bifunctor.Extended         (first)
import qualified Data.Binary                     as Binary
import qualified Data.ByteString.Lazy            as BL
import           Data.Function                   (on)
import qualified Data.HashMap.Strict             as HMS
import qualified Data.HashSet.Extended           as HS
import           Data.IORef.Extended             (IORef)
import qualified Data.IORef.Extended             as IORef
import           Data.Maybe                      (fromMaybe, mapMaybe,
                                                  maybeToList)
import           Data.Memoize                    (memoize)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import qualified Data.Text.Lazy                  as TL
import qualified Data.Text.Lazy.Encoding         as TL
import           Data.Traversable.HigherOrder    (htraverse)
import qualified Data.Yaml.Extended              as Yaml
import           Fregot.Builtins                 (Builtin, Builtins,
                                                  defaultBuiltins)
import qualified Fregot.Compile.Graph            as Compile
import           Fregot.Compile.Package          (CompiledRule)
import qualified Fregot.Compile.Package          as Compile
import           Fregot.Error                    (Error, catchIO)
import qualified Fregot.Error                    as Error
import qualified Fregot.Error.Stack              as Stack
import qualified Fregot.Eval                     as Eval
import qualified Fregot.Eval.Cache               as Cache
import qualified Fregot.Eval.Json                as Eval.Json
import           Fregot.Eval.Monad               (RuleCache)
import           Fregot.Eval.Value               (emptyObject)
import           Fregot.Interpreter.Bundle
import qualified Fregot.Interpreter.Dependencies as Deps
import           Fregot.Names
import qualified Fregot.Names.Renamer            as Renamer
import qualified Fregot.Parser                   as Parser
import qualified Fregot.Prepare                  as Prepare
import           Fregot.Prepare.Ast              (Function (..), Query, Term)
import qualified Fregot.Prepare.Json             as Prepare
import qualified Fregot.Prepare.Package          as Prepare
import qualified Fregot.Prepare.Yaml             as Prepare
import           Fregot.PrettyPrint              ((<$$>), (<+>))
import qualified Fregot.PrettyPrint              as PP
import qualified Fregot.Sources                  as Sources
import           Fregot.Sources.SourceSpan       (SourcePointer, SourceSpan,
                                                  sourcePointer)
import qualified Fregot.Sugar                    as Sugar
import qualified Fregot.Tree                     as Tree
import qualified Fregot.Types.Internal           as Types
import qualified Fregot.Types.Value              as Types
import           System.Directory                (canonicalizePath)
import           System.FilePath.Extended        (listExtensions)

type InterpreterM a = ParachuteT Error IO a

data Handle = Handle
    { _builtins :: !(IORef (Builtins Identity))
    , _sources  :: !Sources.Handle
    -- | List of modules, and files we loaded them from.  Grouped by package
    -- name.
    , _modules  :: !(IORef (HMS.HashMap PackageName ModuleBatch))
    , _yamls    :: !(IORef (HMS.HashMap FilePath Prepare.PreparedTree))
    -- | Tree of compiled rules.
    , _ruleTree :: !(IORef (Tree.Tree CompiledRule))
    , _cache    :: !(IORef RuleCache)
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
    _yamls        <- IORef.newIORef HMS.empty
    _ruleTree     <- IORef.newIORef Tree.empty
    _cache        <- Cache.new >>= IORef.newIORef
    _inputDoc     <- IORef.newIORef emptyObject
    return Handle {..}

-- | Create a new copy of this interpreter.  Copies all internal handles and
-- clears the cache.
copyHandle :: Handle -> IO Handle
copyHandle h = do
    _sources  <- copy (h ^. sources)
    _builtins <- copy (h ^. builtins)
    _modules  <- copy (h ^. modules)
    _yamls    <- copy (h ^. yamls)
    _ruleTree <- copy (h ^. ruleTree)
    _cache    <- Cache.new >>= IORef.newIORef
    _inputDoc <- copy (h ^. inputDoc)
    return Handle {..}
  where
    copy ref = IORef.readIORef ref >>= IORef.newIORef

readSources :: Handle -> IO Sources.Sources
readSources h = IORef.readIORef (h ^. sources)

readRuleDependencyGraph
    :: Handle -> IO (Deps.Graph Tree.Key)
readRuleDependencyGraph h = do
    tree <- IORef.readIORef (h ^. ruleTree)
    let dependencies = memoize $ \k -> maybe
            [] (HS.toList . Compile.ruleDependencies tree) (Tree.lookup k tree)
    return $ Deps.Graph
        { Deps.graphDone         = Tree.keys tree
        , Deps.graphIsDone       = \k -> Tree.member k tree
        , Deps.graphDependencies = dependencies
        }

-- | Auxiliary function for hooking into the renamer.
runRenamerT :: Renamer.RenamerEnv -> Renamer.RenamerM a -> InterpreterM a
runRenamerT renv = mapParachuteT (return . flip runReader renv)

-- | This is a bit hacky and we should probably get rid of it.
universeForRenamer
    :: Handle -> InterpreterM (PackageName -> [UnqualifiedVar])
universeForRenamer h = do
    modmap <- liftIO $ IORef.readIORef (h ^. modules)
    yamls0 <- liftIO $ IORef.readIORef (h ^. yamls)

    let packageTree = Tree.fromList
            [ (review packageNameFromKey key, ())
            | key <- modmap ^.. to HMS.toList . traverse . _1
            ]
    pure $ \pkgname ->
        -- Rule bits.
        (modmap ^.. ix pkgname . traverse . Sugar.moduleRuleNames) ++
        -- Dynamic references to packages.
        (do
            let key = review packageNameFromKey pkgname
            pkg <- maybeToList $ Tree.descendant key packageTree
            map fst $ Tree.children pkg) ++
        -- Yaml bits.
        (do
            let key = review packageNameFromKey pkgname
            yaml <- map snd $ HMS.toList yamls0
            pkg  <- maybeToList $ Tree.descendant key yaml
            map fst $ Tree.children pkg)

insertModule
    :: Handle -> Sugar.Module SourceSpan Var -> InterpreterM ()
insertModule h modul = do
    -- We replace the module that has the same source pointer.
    let replaced = on (==) (^. Sugar.moduleAnn . sourcePointer) modul

    -- Insert or replace the module.  Return the rules that were removed.
    let pkgname = modul ^. Sugar.modulePackage
    oldRules <- liftIO $ IORef.atomicModifyIORef' (h ^. modules) $ \mods ->
        let m1  = fromMaybe [] (HMS.lookup pkgname mods)
            m2  = modul : filter (not . replaced) m1
            old = map ((,) pkgname) $ case HMS.lookup pkgname mods of
                Nothing  -> []
                Just pkg -> pkg ^.. moduleBatchRules in
        (HMS.insert pkgname m2 mods, old)

    evictRules h oldRules
    dieIfErrors
  where
    -- All rules defined in a 'ModuleBatch'.
    moduleBatchRules = traverse . Sugar.moduleRuleNames

evictRules :: Handle -> [QualifiedVar] -> InterpreterM ()
evictRules h victims = do
    -- Compute all rules that depended on rules in the module, so we can evict
    -- all of them.
    ruleDepGraph <- liftIO (readRuleDependencyGraph h)
    let transitive = Deps.evict ruleDepGraph $ HS.fromList $
            map (review Tree.qualifiedVarFromKey) victims
    unless (HS.null transitive) $ bumpCache h
    -- Remove everything that's in the evict set.
    liftIO $ IORef.atomicModifyIORef_ (h ^. ruleTree) $
        Tree.filterWithKey (\k _ -> not (k `HS.member` transitive))

loadModule
    :: Handle -> Parser.ParserOptions -> FilePath -> InterpreterM PackageName
loadModule h popts path = do
    -- Read the source code and parse the module.
    input <- catchIO $ T.readFile path
    liftIO $ IORef.atomicModifyIORef_ (h ^. sources) $
        Sources.insert sourcep input
    modul <- Parser.lexAndParse (Parser.parseModule popts) sourcep input

    -- Insert or replace the module.
    insertModule h modul
    return $ modul ^. Sugar.modulePackage
  where
    sourcep = Sources.FileInput path

loadData
    :: Handle -> DestinationPrefix FilePath
    -> (forall m. Monad m
        => PackageName -> SourcePointer -> T.Text
        -> ParachuteT Error m Prepare.PreparedTree)
    -> InterpreterM ()
loadData h (DestinationPrefix destination path) mkTree = do
    canonical <- catchIO $ liftIO $ canonicalizePath path
    input     <- catchIO $ T.readFile path
    liftIO $ IORef.atomicModifyIORef_ (h ^. sources) $
        Sources.insert sourcep input

    tree <- mkTree destination sourcep input
    oldRules <- liftIO $ IORef.atomicModifyIORef' (h ^. yamls) $ \ys ->
        case HMS.lookup canonical ys of
            Nothing -> (HMS.insert canonical tree ys, mempty)
            Just t  -> (HMS.insert canonical tree ys, Tree.keys t)
    evictRules h $ mapMaybe (preview qualifiedVarFromKey) oldRules
  where
    sourcep = Sources.FileInput path

loadYaml
    :: Handle -> DestinationPrefix FilePath -> InterpreterM ()
loadYaml h path = loadData h path $ \pkgname sourcep ->
    either fatal pure . Prepare.loadYaml pkgname sourcep .
    TL.encodeUtf8 .  TL.fromStrict

loadJson
    :: Handle -> DestinationPrefix FilePath -> InterpreterM ()
loadJson h path = loadData h path Prepare.loadJson

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
                \modul -> insertModule h modul
            return $ HMS.keys $ bundle ^. bundleModules

loadFileByExtension
    :: Handle -> Parser.ParserOptions -> DestinationPrefix FilePath
    -> InterpreterM (Maybe PackageName)
loadFileByExtension h popts path = case listExtensions (extract path) of
    "rego" : _            ->
        assertNoDestinationPrefix >> Just <$> loadModule h popts (extract path)
    "bundle" : "rego" : _ ->
        assertNoDestinationPrefix >> Nothing <$ loadBundle h (extract path)
    "yaml" : _            -> Nothing <$ loadYaml h path
    "yml" : _             -> Nothing <$ loadYaml h path
    "json" : _            -> Nothing <$ loadJson h path
    _                     -> fatal $ Error.mkErrorNoMeta "interpreter" $
        "Unknown rego file extension:" <+> PP.pretty' (extract path) <>
        ", expected .rego or .bundle.rego"
  where
    assertNoDestinationPrefix = when (hasDestinationPrefix path) $
        fatal $ Error.mkErrorNoMeta "interpreter" $
        "Package name destination prefixes are only supported" <+>
        "for data files, not rego files or bundles."

saveBundle :: Handle -> FilePath -> InterpreterM ()
saveBundle h path = liftIO $ do
    _bundleSources <- IORef.readIORef (h ^. sources)
    _bundleModules <- IORef.readIORef (h ^. modules)
    BL.writeFile path $ GZip.compress $ Binary.encode $ Bundle {..}

-- | Compile a specific package.  This will compile its dependencies first.
compileRules
    :: Handle -> InterpreterM ()
compileRules h = do
    tree0    <- liftIO $ IORef.readIORef (h ^. ruleTree)
    modmap   <- liftIO $ IORef.readIORef (h ^. modules)
    yamls0   <- liftIO $ IORef.readIORef (h ^. yamls)
    builtin  <- liftIO $ IORef.readIORef (h ^. builtins)
    universe <- universeForRenamer h

    -- Find missing rules.  This would probably be better expressed as a tree
    -- difference in a different module.
    let (wantedPkgs, _wantedKeys) = first HS.fromList $ unzip $ do
            (_, mods) <- HMS.toList modmap
            modul     <- mods
            name      <- modul ^.. Sugar.moduleRuleNames
            let key = review Tree.qualifiedVarFromKey
                        (modul ^. Sugar.modulePackage, name)
            guard $ not $ key `Tree.member` tree0
            pure (modul ^. Sugar.modulePackage, key)

    -- Get a tree from the YAML files.
    yamlTree <- Prepare.mergeTrees
        [t `Tree.difference` tree0 | (_, t) <- HMS.toList yamls0]

    -- Start by preparing the necessary packages a a big tree.  Note that we are
    -- currently compiling more rules than we strictly need to!  We should
    -- figure out a way to only compile `wantedKeys`.
    preparedTree <- foldM
        (\t pkgname -> do
            let -- Find the modules the prepare; more than one module may be
                -- listed under the given package name.
                mods = fromMaybe [] (HMS.lookup pkgname modmap)

            -- Rename the found moduless.
            renamed <- Renamer.renameModules builtin pkgname
                    universe
                    mods

            -- Prepare the renamed modules and then merge this in.
            t' <- Prepare.prepareModules renamed
            Prepare.mergeTree t t')

        yamlTree wantedPkgs

    -- Compile the tree and save it.
    tree1 <- Compile.compileTree builtin tree0 preparedTree
    dieIfErrors
    liftIO $ IORef.writeIORef (h ^. ruleTree) tree1

-- | Get a list of all builtins.
readBuiltins :: Handle -> InterpreterM [Function]
readBuiltins h = liftIO $ HMS.keys <$> IORef.readIORef (h ^. builtins)

-- | Register a builtin.
insertBuiltin :: Handle -> Function -> Builtin Identity -> InterpreterM ()
insertBuiltin h k b =
    liftIO $ IORef.atomicModifyIORef_ (h ^. builtins) $ HMS.insert k b

-- | Get a list of loaded packages.
--
-- TODO(jaspervdj): This interface is wrong; we should be browsing the tree
-- instead.
readPackages :: Handle -> InterpreterM [PackageName]
readPackages h =
    liftIO $ HMS.keys <$> IORef.readIORef (h ^. modules)

-- | Read all rules in a specific package.
readPackageRules :: Handle -> PackageName -> InterpreterM [Var]
readPackageRules h pkgname = do
    tree <- liftIO $ IORef.readIORef (h ^. ruleTree)
    let key = review Tree.packageNameFromKey pkgname
    return $ case Tree.descendant key tree of
        Nothing  -> []
        Just pkg -> map fst (Tree.children pkg)

-- | Read all available rules.  This is used to enumerate all rules starting
-- with `test_` by the tester.
readAllRules :: Handle -> InterpreterM [(PackageName, Var)]
readAllRules h = do
    tree <- liftIO $ IORef.readIORef (h ^. ruleTree)
    return $ do
        (key, _)        <- Tree.toList tree
        (pkgname, rule) <- maybeToList $ preview Tree.qualifiedVarFromKey key
        return (pkgname, rule)

insertRule
    :: Handle -> PackageName -> Sugar.Rule SourceSpan Var -> InterpreterM ()
insertRule h pkgname rule = insertModule h Sugar.Module
    { _moduleAnn     = rule ^. Sugar.ruleHead . Sugar.ruleAnn
    , _modulePackage = pkgname
    , _moduleImports = []  -- TODO(jaspervdj): REPL imports here?
    , _modulePolicy  = [rule]
    }

newEvalContext :: Handle -> InterpreterM Eval.EnvContext
newEvalContext h = Eval.EnvContext
    <$> pure Eval.emptyContext
    <*> (Eval.Environment
        <$> liftIO (IORef.readIORef (h ^. builtins))
        <*> liftIO (IORef.readIORef (h ^. ruleTree))
        <*> liftIO (IORef.readIORef (h ^. inputDoc))
        <*> liftIO (IORef.readIORef (h ^. cache))
        <*> Cache.new
        <*> pure Stack.empty)

eval
    :: Handle -> Maybe Eval.EnvContext -> PackageName -> Eval.EvalM a
    -> InterpreterM (Eval.Document a)
eval h mbEvalEnvCtx _pkgname mx = do
    envctx <- maybe (newEvalContext h) return mbEvalEnvCtx
    either fatal return =<< liftIO (Eval.runEvalM envctx mx)

evalQuery
    :: Handle -> Maybe Eval.EnvContext -> PackageName
    -> Sugar.Query SourceSpan Var -> InterpreterM (Eval.Document Eval.Value)
evalQuery h mbEvalEnvCtx pkgname query = do
    cquery <- compileQuery h mbEvalEnvCtx pkgname query
    eval h mbEvalEnvCtx pkgname (Eval.evalQuery cquery)

evalVar
    :: Handle -> Maybe Eval.EnvContext -> SourceSpan -> PackageName -> Var
    -> InterpreterM (Eval.Document Eval.Value)
evalVar h mbEvalEnvCtx source pkgname var = do
    let expr = Sugar.TermE source (Sugar.VarT source var)
    evalQuery h mbEvalEnvCtx pkgname $
        review (Sugar.literalFromQuery . Sugar.exprFromLiteral) expr

newResumeStep
    :: Handle -> PackageName -> Sugar.Query SourceSpan Var
    -> InterpreterM (Eval.ResumeStep Eval.Value)
newResumeStep h pkgname query = do
    -- Disable the cache for evaluating queries in resume steps.
    -- TODO(jaspervdj): Disable `comprehensionCache`?
    envctx   <- over (Eval.ecEnvironment . Eval.ruleCache) Cache.disable <$>
                newEvalContext h
    ctree    <- liftIO $ IORef.readIORef (h ^. ruleTree)
    universe <- universeForRenamer h

    -- Rename expression.
    let builtin    = envctx ^. Eval.ecEnvironment . Eval.builtins
        renamerEnv = Renamer.RenamerEnv
            builtin
            mempty  -- No imports?
            pkgname
            (HS.fromList (universe pkgname))
            -- TODO(jaspervdj): This is where we would want to grab the tree
            -- from within the environment?
            -- (envctx ^. Eval.ecEnvironment . Eval.packages)
            universe
            HS.empty
    rquery <- runRenamerT renamerEnv $ Renamer.renameQuery query

    pquery <- Prepare.prepareQuery rquery
    cquery <- Compile.compileQuery builtin ctree mempty pquery

    dieIfErrors
    return $ Eval.newResumeStep envctx (Eval.evalQuery cquery)

step
    :: Handle -> Eval.ResumeStep Eval.Value
    -> InterpreterM (Eval.Step Eval.Value)
step _ = liftIO . Eval.runStep

bumpCache :: Handle -> InterpreterM ()
bumpCache h = liftIO $ IORef.readIORef (h ^. cache) >>=
    Cache.bump >>= IORef.writeIORef (h ^. cache)

setInput :: Handle -> Aeson.Value -> InterpreterM ()
setInput h val = do
    bumpCache h
    liftIO $ IORef.writeIORef (h ^. inputDoc) (Eval.Json.toValue val)

setInputFile :: Handle -> FilePath -> InterpreterM ()
setInputFile h path = do
    errOrVal <- liftIO $ Yaml.loadJsonOrYaml path
    val      <- case errOrVal of
        Right v  -> pure (v :: Aeson.Value)
        Left err -> fatal $ Error.mkErrorNoMeta "interpreter" $
            "Loading input file" <+> PP.pretty path <+> "failed:" <$$>
            PP.ind (PP.pretty err)
    setInput h val

compileQuery
    :: Handle -> Maybe Eval.EnvContext -> PackageName
    -> Sugar.Query SourceSpan Var
    -> InterpreterM (Query SourceSpan)
compileQuery h mbEvalEnvCtx pkgname query = do
    ctree    <- liftIO $ IORef.readIORef (h ^. ruleTree)
    builtin  <- liftIO $ IORef.readIORef (h ^. builtins)
    universe <- universeForRenamer h

    -- Rename expression.
    let renamerEnv = Renamer.RenamerEnv
            builtin
            mempty  -- No imports?
            pkgname
            (HS.fromList $ universe pkgname)
            universe
            HS.empty
    rquery <- runRenamerT renamerEnv $ Renamer.renameQuery query

    pquery <- Prepare.prepareQuery rquery
    cquery <- Compile.compileQuery builtin ctree typeContext pquery
    dieIfErrors
    return cquery
  where
    typeContext = case mbEvalEnvCtx of
        Nothing -> mempty
        Just ec -> Types.inferContext $ ec ^. Eval.ecContext

compileExpr
    :: Handle -> Maybe Eval.EnvContext -> PackageName
    -> Sugar.Expr SourceSpan Var
    -> InterpreterM (Term SourceSpan, Types.Type)
compileExpr h mbEvalEnvCtx pkgname expr = do
    ctree    <- liftIO $ IORef.readIORef (h ^. ruleTree)
    builtin  <- liftIO $ IORef.readIORef (h ^. builtins)
    universe <- universeForRenamer h

    -- Rename expression.
    let renamerEnv = Renamer.RenamerEnv
            builtin
            mempty  -- No imports?
            pkgname
            (HS.fromList $ universe pkgname)
            universe
            HS.empty
    rterm <- runRenamerT renamerEnv $ Renamer.renameExpr expr

    pterm       <- Prepare.prepareExpr rterm
    (cterm, ty) <- Compile.compileTerm builtin ctree typeContext pterm
    dieIfErrors
    return (cterm, fst ty)
  where
    typeContext = case mbEvalEnvCtx of
        Nothing -> mempty
        Just ec -> Types.inferContext $ ec ^. Eval.ecContext

typeExpr
    :: Handle -> Maybe Eval.EnvContext -> PackageName
    -> Sugar.Expr SourceSpan Var -> InterpreterM Types.Type
typeExpr h mbEvalEnvCtx pkgname expr =
    snd <$> compileExpr h mbEvalEnvCtx pkgname expr
