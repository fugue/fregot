-- | This module optimistically renames name in a program to their fully
-- qualified versions.
--
-- This is not always possible, and at this phase, we don't know about local
-- variables yet (that happens later in the safe variable analysis).
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
    ) where

import           Control.Lens              (locally, view, (^.), (^..), _2)
import           Control.Lens.TH           (makeLenses)
import           Control.Monad             (guard)
import           Control.Monad.Parachute   (ParachuteT, fatal, mapParachuteT,
                                            tellError)
import           Control.Monad.Reader      (Reader, runReader)
import           Data.Bifunctor            (first)
import qualified Data.HashMap.Strict       as HMS
import qualified Data.HashSet.Extended     as HS
import           Data.List.Extended        (splits, unsnoc)
import           Data.Maybe                (listToMaybe, maybeToList)
import           Data.Traversable          (for)
import           Fregot.Error              (Error)
import qualified Fregot.Error              as Error
import           Fregot.Eval.Builtins      (ReadyBuiltin)
import           Fregot.Names
import           Fregot.Names.Imports
import           Fregot.Prepare.Ast        (Function (..), Imports)
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
    localVars = HS.toHashSetOf
        (traverse . traverse . _VarDeclS . _2 . traverse)
        bodies

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
    <*> pure (rh ^. ruleName)
    <*> traverse (traverse renameTerm) (rh ^. ruleArgs)
    <*> traverse renameTerm (rh ^. ruleIndex)
    <*> traverse renameTerm (rh ^. ruleValue)

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
    VarDeclS a vs -> pure (VarDeclS a vs)
    LiteralS lit  -> LiteralS <$> renameLiteral lit

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

specialBuiltinVar :: Var -> Bool
specialBuiltinVar "data"  = True
specialBuiltinVar "input" = True
specialBuiltinVar _       = False

-- | Resolves a reference as far as possible.  Returns the new name, and
-- bits that were not resolved yet.
--
-- For example, `foo` is a package, and `foo.bar` is a rule in that package
-- that evaluates to `foo.bar = {"value": 1}`.
--
-- We resolve the `foo.bar` part in `foo.bar.value` and keep the `.value`
-- part unresolved, then "attach it back on".
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

        "data"  | Just (pkg, rname, remainder) <-
                    resolveData thispkg universe refArgs ->
            checkExists pkg rname $ do
                remainder' <- traverse renameRefArg remainder
                pure $ Just (mkQualifiedName pkg rname, remainder')

        _       | Just (_, ImportData pkg) <- HMS.lookup var imports
                , (ra1 : ras)              <- refArgs
                , Just rname               <- refArgToVar ra1 ->
            checkExists pkg rname $ do
            ras' <- traverse renameRefArg ras
            pure $ Just (mkQualifiedName pkg rname, ras')

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

        -- Nothing was found.  We will assume it is a local var.
        _ -> do
            refArgs' <- traverse renameRefArg refArgs
            let name
                    | specialBuiltinVar var  = BuiltinName var
                    | HS.member var rules    = mkQualifiedName thispkg var
                    | var `HS.member` locals = LocalName var
                    | otherwise              = LocalName var
            return $ Just (name, refArgs')

  where
    refArgToVar = \case
        RefBrackArg (TermE _ (ScalarT _ (String k))) -> Just (mkVar k)
        RefDotArg _ k                                -> Just k
        _                                            -> Nothing

    -- Hidden in this body because we don't want to naively call it.
    renameRefArg = \case
        RefBrackArg e  -> RefBrackArg <$> renameExpr e
        RefDotArg s uv -> pure (RefDotArg s uv)

    resolveData thispkg universe args = listToMaybe $ do
        -- The reverse here is used to try the longest path first.
        (pre, (name : remainder)) <- reverse $ splits args
        pkg <- fmap (mkPackageName . map unVar) $ maybeToList $
            mapM refArgToVar pre
        name' <- maybeToList $ refArgToVar name
        guard $ pkg == thispkg || not (null (universe pkg))
        return (pkg, name', remainder)

renameTerm :: Rename Term
renameTerm = \case
    RefT source varSource var refArgs -> do
        mbResolved <- resolveRef source var refArgs
        case mbResolved of
            Just (name, [])       -> pure $ VarT source name
            Just (name, refArgs') -> pure $ RefT source varSource name refArgs'
            Nothing               -> pure $ ErrorT source

    CallT source ns args -> do
        builtins <- view reBuiltins
        imports  <- view reImports
        rules    <- view rePackageRules
        thispkg  <- view rePackage
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
                | Just (_, ImportData pkg) <- HMS.lookup (mkVar imp) imports ->
                    pure $ CallT source [mkQualifiedName pkg n] args'

            _ -> fatal $ Error.mkError "renamer" source "unknown call" $
                -- NOTE(jaspervdj): We can use ErrorT here.
                "Unknown call to" <+> PP.pretty (Nested ns)

    -- Find out of the variable is a rule in this package.  If so, it's a
    -- QualifiedName, if not it's a LocalName.  WildcardName and BuiltinName are
    -- special cases.
    VarT a "_" -> pure $ VarT a WildcardName
    VarT a v | specialBuiltinVar v ->
        pure $ VarT a (BuiltinName v)
    VarT a v -> do
        locals <- view reLocalVars
        pkg    <- view rePackage
        rules  <- view rePackageRules
        case v `HS.member` rules of
            _ | v `HS.member` locals -> pure (VarT a (LocalName v))
            True                     -> pure (VarT a (mkQualifiedName pkg v))
            False                    -> pure (VarT a (LocalName v))

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
