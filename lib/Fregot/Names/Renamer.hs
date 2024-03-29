{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

This module optimistically renames name in a program to their fully
qualified versions.

This is not always possible, and at this phase, we don't know about local
variables yet (that happens later in the safe variable analysis).
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Names.Renamer
    ( -- * Rename a number of modules at once.
      renameModules

      -- * Lower-level functionality.
    , RenamerEnv (..), reBuiltins, reImports, rePackage, rePackageRules
    , reUniverse
    , RenamerM
    , renameModule
    , renameQuery
    , renameExpr

    , termLhsAssignVars
    , exprLhsAssignVars
    ) where

import           Control.Lens              (locally, view, (^.), (^..))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad             (guard)
import           Control.Monad.Parachute   (ParachuteT, fatal, mapParachuteT,
                                            tellError)
import           Control.Monad.Reader      (Reader, runReader)
import           Data.Bifunctor            (first)
import           Data.Hashable             (Hashable)
import qualified Data.HashMap.Strict       as HMS
import qualified Data.HashSet.Extended     as HS
import           Data.List.Extended        (splits, unsnoc)
import           Data.Maybe                (fromMaybe, listToMaybe, maybeToList)
import           Data.Traversable          (for)
import           Fregot.Builtins.Internal  (ReadyBuiltin)
import           Fregot.Error              (Error)
import qualified Fregot.Error              as Error
import           Fregot.Names
import           Fregot.Names.Imports
import           Fregot.Prepare.Ast        (Function (..))
import           Fregot.PrettyPrint        ((<+>))
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Fregot.Sugar

data RenamerEnv = RenamerEnv
    { _reBuiltins     :: !(HMS.HashMap Function ReadyBuiltin)
    , _reImports      :: !(Imports SourceSpan)
    , _rePackage      :: !PackageName
    , _rePackageRules :: !(HS.HashSet Var)
    , _reUniverse     :: !(PackageName -> [UnqualifiedVar])
    , _reLocalVars    :: !(HS.HashSet UnqualifiedVar)
    }

$(makeLenses ''RenamerEnv)

-- | Rename a number modules located under the same package name.
renameModules
    :: Monad m
    => HMS.HashMap Function ReadyBuiltin
    -> PackageName
    -> (PackageName -> [UnqualifiedVar])
    -> Modules SourceSpan Var
    -> ParachuteT Error m (Modules SourceSpan Name)
renameModules builtin pkgname universe modules = for modules $ \modul -> do
    let imports = gatherImports (modul ^. moduleImports)
        env     = RenamerEnv builtin imports pkgname pkgrules universe HS.empty
    mapParachuteT (\mx -> pure (runReader mx env)) (renameModule modul)
  where
    -- All rules in the package.
    pkgrules = HS.toHashSetOf (traverse . moduleRuleNames) modules

type RenamerM a = ParachuteT Error (Reader RenamerEnv) a

type Rename f = f SourceSpan Var -> RenamerM (f SourceSpan Name)

tellRenameError :: SourceSpan -> PP.SemDoc -> PP.SemDoc -> RenamerM ()
tellRenameError source title = tellError .
    Error.mkError "renamer" source title

withLocalVarDecls
    :: [RuleBody SourceSpan Var]  -- ^ Bodies that may have 'some' statements
    -> RenamerM a                 -- ^ Renamer to be run with the local vars
    -> RenamerM a
withLocalVarDecls bodies =
    locally reLocalVars (HS.union localVars)
  where
    localVars :: HS.HashSet UnqualifiedVar
    localVars = foldMap someVars (concat bodies)

    someVars :: RuleStatement SourceSpan Var -> HS.HashSet UnqualifiedVar
    someVars (LiteralS _)         = mempty
    someVars (EveryInS _ _ _ _ _) = mempty
    someVars (SomeS _ vs)         = HS.fromList vs
    someVars (SomeInS _ k v _)    =
        fromMaybe mempty (k >>= termLhsAssignVars) <>
        fromMaybe mempty (termLhsAssignVars v)

renameModule :: Rename Module
renameModule modul = Module
    <$> pure (modul ^. moduleAnn)
    <*> pure (modul ^. modulePackage)
    <*> pure (modul ^. moduleImports)
    <*> traverse renameRule (modul ^. modulePolicy)

renameRule :: Rename Rule
renameRule rule = withLocalVarDecls bodies $ Rule
    <$> renameRuleHead (rule ^. ruleHead)
    <*> traverse renameRuleBody (rule ^. ruleBodies)
    <*> traverse renameRuleElse (rule ^. ruleElses)
  where
    bodies =
        (rule ^. ruleBodies) ++
        (rule ^.. ruleElses . traverse . ruleElseBody)

renameRuleHead :: Rename RuleHead
renameRuleHead rh = RuleHead
    <$> pure (rh ^. ruleAnn)
    <*> pure (rh ^. ruleDefault)
    <*> pure (rh ^. ruleAssign)
    <*> pure (rh ^. ruleName)
    <*> traverse (traverse renameTerm) (rh ^. ruleArgs)
    <*> traverse renameTerm (rh ^. ruleIndex)
    <*> traverse renameExpr (rh ^. ruleValue)

renameRuleBody :: RuleBody SourceSpan Var -> RenamerM (RuleBody SourceSpan Name)
renameRuleBody = traverse renameRuleStatement

renameQuery :: Query SourceSpan Var -> RenamerM (Query SourceSpan Name)
renameQuery = renameRuleBody

renameRuleElse :: Rename RuleElse
renameRuleElse re = RuleElse
    <$> pure (re ^. ruleElseAnn)
    <*> traverse renameTerm (re ^. ruleElseValue)
    <*> renameRuleBody (re ^. ruleElseBody)

renameRuleStatement :: Rename RuleStatement
renameRuleStatement = \case
    SomeS    a vs      -> pure (SomeS a vs)
    LiteralS lit       -> LiteralS <$> renameLiteral lit
    SomeInS  a k v x   ->
        SomeInS a <$> traverse renameTerm k <*> renameTerm v <*> renameExpr x
    EveryInS a k v x b ->
        let everyVars =
                fromMaybe mempty (k >>= termLhsAssignVars) <>
                fromMaybe mempty (termLhsAssignVars v) in
        EveryInS a
            <$> traverse renameTerm k
            <*> renameTerm v
            <*> renameExpr x
            <*> locally reLocalVars (HS.union everyVars) (renameRuleBody b)

renameLiteral :: Rename Literal
renameLiteral lit = Literal
    <$> pure (lit ^. literalAnn)
    <*> pure (lit ^. literalNegation)
    <*> renameExpr (lit ^. literalExpr)
    <*> traverse renameWith (lit ^. literalWith)

renameExpr :: Rename Expr
renameExpr = \case
    TermE a t      -> TermE   a <$> renameTerm t
    BinOpE a x b y -> BinOpE  a <$> renameExpr x <*> pure b <*> renameExpr y
    ParensE a x    -> ParensE a <$> renameExpr x
    IndRefE a x ys -> IndRefE a <$> renameTerm x <*> traverse renameRefArg ys
    InE a k v x    -> InE     a <$>
        traverse renameExpr k <*> renameExpr v <*> renameExpr x

specialBuiltinVar :: Var -> Bool
specialBuiltinVar "data"  = True
specialBuiltinVar "input" = True
specialBuiltinVar _       = False

-- | Resolves a reference as far as possible.  Returns the new name, and
-- bits that were not resolved yet.  You can generally think of as the
-- left part of the tuple as the "statically known" part, and the right
-- part is of the tuple is the "dynamic" part.
--
-- For example, `foo` is a package, and `foo.bar` is a rule in that package
-- that evaluates to `foo.bar = {"value": 1}`.
--
-- We resolve the `foo.bar` part in `foo.bar.value` and keep the `.value`
-- part unresolved, then "attach it back on".
--
-- Why does this code have so many cases?  Well, there's a tradeoff going on
-- here.  We could significantly reduce the number of checks here, which would
-- make the code much simpler.  However, we also want to be able to catch all
-- "this thing is not defined" errors here if possible.
--
-- Catching all errors like that is not possible in general (because of the
-- dynamic part), but we want to at least deal with all the common cases to
-- help policy authors.
resolveRef
    :: SourceSpan
    -> Var -> [RefArg SourceSpan Var]
    -> RenamerM (Maybe (Name, [RefArg SourceSpan Name]))
resolveRef source var refArgs = do
    imports   <- view reImports
    rules     <- view rePackageRules
    thispkg   <- view rePackage
    universe  <- view reUniverse
    locals    <- view reLocalVars

    -- Auxiliary to check if something actually exists in the deps.
    let checkExists pkg _ cont | pkg == thispkg = cont
        checkExists pkg name cont = case universe pkg of
            [] -> do
                tellRenameError source "unknown package" $
                    "Package" <+> PP.pretty pkg <+>
                    "is imported but not loaded (or empty)."
                pure Nothing
            uni | not (name `elem` uni) -> do
                tellRenameError source "unknown function" $
                    "Rule" <+> PP.pretty name <+>
                    "is not defined in package" <+> PP.pretty pkg
                pure Nothing
            _ -> cont

    case var of

        -- A simple wildcard variable.
        "_"     | null refArgs -> pure $ Just (WildcardName, [])

        -- A fully qualified path, e.g. `data.foo.bar`.  Resolve as far
        -- as possible using `resolveData`.
        "data"  | Just (pkg, rname, remainder) <-
                    resolveData thispkg universe refArgs ->
            checkExists pkg rname $ do
                remainder' <- traverse renameRefArg remainder
                pure $ Just (mkQualifiedName pkg rname, remainder')

        -- Using an import, e.g. `foo.bar` where we also have `import data.foo`
        -- or `import data.qux as foo`.  Think of this as "importing a package".
        _       | Just (_, ImportData pkg) <- HMS.lookup var imports
                , (ra1 : ras)              <- refArgs
                , Just rname               <- refArgToVar ra1 ->
            checkExists pkg rname $ do
            ras' <- traverse renameRefArg ras
            pure $ Just (mkQualifiedName pkg rname, ras')

        -- Using an import as a variable, e.g. `bar` where we have
        -- `import data.foo.bar`.
        _       | Just (_, ImportData pkg0) <- HMS.lookup var imports
                , null refArgs
                , pieces@(_ : _) <- unPackageName pkg0 ->
            -- The package name starts out incorrect here, we need to move it
            -- one level up.
            let pkg1 = mkPackageName (init pieces)
                var1 = mkVar (last pieces) in
            checkExists pkg1 var1 $
            pure $ Just (mkQualifiedName pkg1 var1, [])

        -- For input imports, it's relatively simple.  If we have something like
        --
        --     import input.foo as blah
        --
        -- Then `blah.bar` needs to resolve to `input.foo.bar`.  This means we
        -- first put the references mentioned in the import, and then the
        -- remaining ones.
        _       | Just (src, ImportInput pkg) <- HMS.lookup var imports -> do
            let importRefs = map (RefDotArg src . mkVar) (unPackageName pkg)
            refArgs' <- traverse renameRefArg refArgs
            pure $ Just (BuiltinName "input", importRefs ++ refArgs')

        -- Nothing was found: probably a local variable, possibly fresh.
        _ -> do
            refArgs' <- traverse renameRefArg refArgs
            let name
                    | specialBuiltinVar var  = BuiltinName var
                    | var `HS.member` locals = LocalName var
                    | HS.member var rules    = mkQualifiedName thispkg var
                    | otherwise              = LocalName var
            return $ Just (name, refArgs')

  where
    refArgToVar = \case
        RefBrackArg (TermE _ (ScalarT _ (String k))) -> Just (mkVar k)
        RefDotArg _ k                                -> Just k
        _                                            -> Nothing

    -- Hidden in this body because we don't want to naively call it.

    resolveData thispkg universe args = listToMaybe $ do
        -- The reverse here is used to try the longest path first.
        (pre, (name : remainder)) <- reverse $ splits args
        pkg <- fmap (mkPackageName . map unVar) $ maybeToList $
            mapM refArgToVar pre
        name' <- maybeToList $ refArgToVar name
        guard $ pkg == thispkg || not (null (universe pkg))
        return (pkg, name', remainder)

renameRefArg :: Rename RefArg
renameRefArg = \case
    RefBrackArg e  -> RefBrackArg <$> renameExpr e
    RefDotArg s uv -> pure (RefDotArg s uv)

renameTerm :: Rename Term
renameTerm = \case
    RefT source varSource var refArgs -> do
        mbResolved <- resolveRef source var refArgs
        case mbResolved of
            Just (name, [])       -> pure $ VarT source name
            Just (name, refArgs') -> pure $ RefT source varSource name refArgs'
            Nothing               -> pure $ ErrorT source

    VarT source v -> do
        mbResolved <- resolveRef source v []
        case mbResolved of
            Just (name, [])       -> pure $ VarT source name
            Just (name, refArgs') -> pure $ RefT source source name refArgs'
            Nothing               -> pure $ ErrorT source

    CallT source ns args -> do
        builtins <- view reBuiltins
        imports  <- view reImports
        rules    <- view rePackageRules
        thispkg  <- view rePackage
        universe <- view reUniverse
        args'    <- traverse renameExpr args
        case first (map unVar) <$> unsnoc ns of
            Nothing -> do
                tellRenameError source "internal error" "Invalid empty call"
                pure $ ErrorT source

            -- Fully qualified call.
            Just (("data" : pkg), n) ->
                pure $ CallT source [mkQualifiedName (mkPackageName pkg) n] args'

            -- A plain builtin name, e.g. "all".
            Just ([], n) | Just _ <- HMS.lookup (NamedFunction (BuiltinName n)) builtins ->
                pure $ CallT source [BuiltinName n] args'

            -- A qualified builtin name, e.g. "json.unmarshal".
            Just (pkg, n) | Just _ <- HMS.lookup (NamedFunction (mkQualifiedName (mkPackageName pkg) n)) builtins ->
                pure $ CallT source [mkQualifiedName (mkPackageName pkg) n] args'

            -- Calling a rule in the same package.  Functions cannot be local
            -- variables.
            Just ([], n)
                | HS.member n rules ->
                    pure $ CallT source [mkQualifiedName thispkg n] args'
                | otherwise -> do
                    tellRenameError source "unknown function" $
                        "Function" <+> PP.pretty n <+> "is not defined."
                    pure $ ErrorT source

            -- Calling a rule in another package.
            Just ([imp], n)
                | Just (_, ImportData pkg) <- HMS.lookup (mkVar imp) imports
                , n `elem` universe pkg ->
                    pure $ CallT source [mkQualifiedName pkg n] args'
                | Nothing <- HMS.lookup (mkVar imp) imports -> do
                    tellRenameError source "unknown function" $
                        "Package" <+> PP.pretty imp <+> "is not imported."
                    pure $ ErrorT source

            _ -> fatal $ Error.mkError "renamer" source "unknown call" $
                -- NOTE(jaspervdj): We can use ErrorT here.
                "Unknown call to" <+> PP.pretty (Nested ns)

    ScalarT a s -> pure (ScalarT a s)

    ArrayT  a xs -> ArrayT  a <$> traverse renameExpr xs
    SetT    a xs -> SetT    a <$> traverse renameExpr xs
    ObjectT a xs -> ObjectT a <$> renameObject xs

    ArrayCompT  a   x b -> withLocalVarDecls [b] $
        ArrayCompT a <$> renameTerm x <*> renameRuleBody b
    SetCompT    a   x b -> withLocalVarDecls [b] $
        SetCompT a <$> renameTerm x <*> renameRuleBody b
    ObjectCompT a k x b -> withLocalVarDecls [b] $
        ObjectCompT a
            <$> renameObjectKey k <*> renameTerm x <*> renameRuleBody b

    ErrorT a -> pure $ ErrorT a

renameObject :: Object SourceSpan Var -> RenamerM (Object SourceSpan Name)
renameObject = traverse renameItem
  where
    renameItem (k, v) = (,) <$> renameObjectKey k <*> renameExpr v

renameObjectKey :: Rename ObjectKey
renameObjectKey = \case
    ScalarK a s -> pure (ScalarK a s)
    VarK a uv -> pure (VarK a uv)
    RefK source var refArgs -> do
        mbResolved <- resolveRef source var refArgs
        case mbResolved of
            Just (name, refArgs') -> pure $ RefK source name refArgs'
            Nothing               -> pure $ ErrorK source
    ErrorK a -> pure (ErrorK a)

renameWith :: Rename With
renameWith with = With
    <$> pure (with ^. withAnn)
    <*> pure (with ^. withPath)
    <*> renameTerm (with ^. withAs)

-- | Computes the variables in the left hand side of a `:=` or `some in` we
-- can assign to.  If we can't assign to a specific term, returns Nothing.
termLhsAssignVars :: (Eq n, Hashable n) => Term a n -> Maybe (HS.HashSet n)
termLhsAssignVars = \case
    RefT         _ _ _ _ -> Nothing
    CallT        _ _ _   -> Nothing
    VarT         _ v     -> Just $ HS.singleton v
    ScalarT      _ _     -> Just mempty
    ArrayT       _ xs    -> HS.unions <$> traverse exprLhsAssignVars xs
    SetT         _ _     -> Nothing
    ObjectT      _ _     -> Nothing
    ArrayCompT   _ _ _   -> Nothing
    SetCompT     _ _ _   -> Nothing
    ObjectCompT  _ _ _ _ -> Nothing
    ErrorT       _       -> Just mempty

exprLhsAssignVars :: (Eq n, Hashable n) => Expr a n -> Maybe (HS.HashSet n)
exprLhsAssignVars = \case
    TermE   _ t     -> termLhsAssignVars t
    BinOpE  _ _ _ _ -> Nothing
    ParensE _ e     -> exprLhsAssignVars e
    IndRefE _ _ _   -> Nothing
    InE     _ _ _ _ -> Nothing
