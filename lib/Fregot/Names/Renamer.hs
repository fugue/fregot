{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Names.Renamer
    ( RenamerEnv (..), reBuiltins, reImports, rePackage, rePackageRules
    , RenamerM
    , renameModule
    , renameExpr
    ) where

import           Control.Lens              (view, (^.))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad.Parachute   (ParachuteT, tellError)
import           Control.Monad.Reader      (Reader)
import           Data.Bifunctor            (first)
import qualified Data.HashMap.Strict       as HMS
import qualified Data.HashSet              as HS
import           Data.List.Extended        (unsnoc)
import           Fregot.Error              (Error)
import qualified Fregot.Error              as Error
import           Fregot.Eval.Builtins      (Builtin)
import           Fregot.Names
import           Fregot.Prepare.Ast        (Function (..))
import           Fregot.PrettyPrint        ((<+>))
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Fregot.Sugar

data RenamerEnv = RenamerEnv
    { _reBuiltins     :: !(HMS.HashMap Function Builtin)
    , _reImports      :: !(Imports SourceSpan)
    , _rePackage      :: !PackageName
    , _rePackageRules :: !(HS.HashSet Var)
    }

$(makeLenses ''RenamerEnv)

type RenamerM a = ParachuteT Error (Reader RenamerEnv) a

type Rename f = f SourceSpan Var -> RenamerM (f SourceSpan Name)

tellRenameError :: SourceSpan -> PP.SemDoc -> PP.SemDoc -> RenamerM ()
tellRenameError source title = tellError .
    Error.mkError "renamer" source title

renameModule :: Rename Module
renameModule modul = Module
    <$> pure (modul ^. modulePackage)
    <*> pure (modul ^. moduleImports)
    <*> traverse renameRule (modul ^. modulePolicy)

renameRule :: Rename Rule
renameRule rule = Rule
    <$> renameRuleHead (rule ^. ruleHead)
    <*> traverse renameRuleBody (rule ^. ruleBodies)
    <*> traverse renameRuleElse (rule ^. ruleElses)

renameRuleHead :: Rename RuleHead
renameRuleHead rh = RuleHead
    <$> pure (rh ^. ruleAnn)
    <*> pure (rh ^. ruleDefault)
    <*> pure (rh ^. ruleName)
    <*> traverse (traverse renameTerm) (rh ^. ruleArgs)
    <*> traverse renameTerm (rh ^. ruleIndex)
    <*> traverse renameTerm (rh ^. ruleValue)

renameRuleBody :: RuleBody SourceSpan Var -> RenamerM (RuleBody SourceSpan Name)
renameRuleBody = traverse renameLiteral

renameRuleElse :: Rename RuleElse
renameRuleElse re = RuleElse
    <$> pure (re ^. ruleElseAnn)
    <*> traverse renameTerm (re ^. ruleElseValue)
    <*> renameRuleBody (re ^. ruleElseBody)

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

-- | Resolves a reference as far as possible.  Returns the new name, and
-- bits that were not resolved yet.
--
-- For example, `foo` is a package, and `foo.bar` is a rule in that package
-- that evaluates to `foo.bar = {"value": 1}`.
--
-- We resolve the `foo.bar` part in `foo.bar.value` and keep the `.value`
-- part unresolved, then "attach it back on".
resolveRef
    :: Var -> [RefArg SourceSpan Var]
    -> RenamerM (Name, [RefArg SourceSpan Name])
resolveRef var refArgs = do
    imports <- view reImports
    rules   <- view rePackageRules
    thispkg <- view rePackage
    case var of
        _       | Just (_, pkg) <- HMS.lookup var imports
                , (ra1 : ras)   <- refArgs
                , Just rname    <- refArgToVar ra1 -> do
            -- TODO(jaspervdj): Check that the name actually exists in the
            -- package?
            ras' <- traverse renameRefArg ras
            return (QualifiedName pkg rname, ras')

        -- Nothing was found.  Should we error here?
        _ -> do
            refArgs' <- traverse renameRefArg refArgs
            let name
                    | HS.member var rules = QualifiedName thispkg var
                    | otherwise           = LocalName var
            return (name, refArgs')

  where
    refArgToVar = \case
        RefBrackArg (TermE _ (ScalarT _ (String k))) -> Just (mkVar k)
        RefDotArg _ k                                -> Just k
        _                                            -> Nothing

    -- Hidden in this body because we don't want to naively call it.
    renameRefArg = \case
        RefBrackArg e  -> RefBrackArg <$> renameExpr e
        RefDotArg s uv -> pure (RefDotArg s uv)

renameTerm :: Rename Term
renameTerm = \case
    RefT source varSource var refArgs -> do
        (name, refArgs') <- resolveRef var refArgs
        case refArgs' of
            [] -> pure $ VarT source name
            _  -> pure $ RefT source varSource name refArgs'

    CallT s ns args -> do
        builtins <- view reBuiltins
        imports  <- view reImports
        rules    <- view rePackageRules
        thispkg  <- view rePackage
        args'    <- traverse renameTerm args
        case first (map unVar) <$> unsnoc ns of
            Nothing -> do
                tellRenameError s "empty call" "Invalid empty call"
                -- TODO(jaspervdj): Return ErrorT?
                pure $ CallT s [] args'

            -- A plain builtin name, e.g. "all".
            Just ([], n) | Just _ <- HMS.lookup (NamedFunction (BuiltinName n)) builtins ->
                pure $ CallT s [BuiltinName n] args'

            -- A qualified builtin name, e.g. "json.unmarshal".
            Just (pkg, n) | Just _ <- HMS.lookup (NamedFunction (QualifiedName (mkPackageName pkg) n)) builtins ->
                pure $ CallT s [QualifiedName (mkPackageName pkg) n] args'

            -- Calling a rule in the same package.
            Just ([], n)
                | HS.member n rules ->
                    pure $ CallT s [QualifiedName thispkg n] args'
                | otherwise -> do
                    tellRenameError s "unknown function" $
                        "Function" <+> PP.pretty n <+> "is not defined."
                    -- TODO(jaspervdj): Return ErrorT?
                    pure $ CallT s [QualifiedName thispkg n] args'

            -- Calling a rule in another package.
            Just ([imp], n) | Just (_, pkg) <- HMS.lookup (mkVar imp) imports ->
                pure $ CallT s [QualifiedName pkg n] args'

            _ -> error "TODO: other calls"

    -- Find out of the variable is a rule in this package.  If so, it's a
    -- QualifiedName, if not it's a LocalName.
    VarT a v -> do
        pkg   <- view rePackage
        rules <- view rePackageRules
        case v `HS.member` rules of
            True  -> pure (VarT a (QualifiedName pkg v))
            False -> pure (VarT a (LocalName v))

    ScalarT a s -> pure (ScalarT a s)

    ArrayT  a xs -> ArrayT  a <$> traverse renameExpr xs
    SetT    a xs -> SetT    a <$> traverse renameExpr xs
    ObjectT a xs -> ObjectT a <$> renameObject xs

    ArrayCompT  a   x b -> ArrayCompT  a <$> renameTerm x <*> renameRuleBody b
    SetCompT    a   x b -> SetCompT    a <$> renameTerm x <*> renameRuleBody b
    ObjectCompT a k x b -> ObjectCompT a <$>
        renameObjectKey k <*> renameTerm x <*> renameRuleBody b

renameObject :: Object SourceSpan Var -> RenamerM (Object SourceSpan Name)
renameObject = traverse renameItem
  where
    renameItem (k, v) = (,) <$> renameObjectKey k <*> renameExpr v

renameObjectKey :: Rename ObjectKey
renameObjectKey = \case
    ScalarK a s -> pure (ScalarK a s)
    -- TODO(jaspervdj): Can we have a rule name here?
    VarK a uv -> pure (VarK a uv)
    RefK source var refArgs -> do
        (name, refArgs') <- resolveRef var refArgs
        return $ RefK source name refArgs'

renameWith :: Rename With
renameWith with = With
    <$> pure (with ^. withAnn)
    <*> pure (with ^. withWith)
    <*> renameTerm (with ^. withAs)
