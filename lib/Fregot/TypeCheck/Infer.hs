{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Fregot.TypeCheck.Infer
    ( TypeError (..)

    , InferEnv (..), ieBuiltins, ieDependencies, ieThisPackage
    , InferM
    , runInfer

    , inferRule
    ) where

import           Control.Lens                  (forOf_, view, (&), (.~), (^.))
import           Control.Lens.TH               (makeLenses, makePrisms)
import           Control.Monad.Except.Extended (catching, throwError)
import           Control.Monad.Parachute       (ParachuteT, fatal)
import           Control.Monad.Reader          (ReaderT, runReaderT)
import           Control.Monad.State.Strict    (StateT, evalStateT, get, modify,
                                                put)
import           Data.Foldable                 (for_)
import qualified Data.HashMap.Strict           as HMS
import           Data.List.NonEmpty.Extended   (NonEmpty)
import qualified Data.List.NonEmpty.Extended   as NonEmpty
import           Data.Maybe                    (fromMaybe)
import           Data.Proxy                    (Proxy)
import qualified Data.Unification              as Unify
import           Fregot.Arity
import           Fregot.Error                  (Error)
import qualified Fregot.Error                  as Error
import           Fregot.Eval.Builtins          (Builtin, Builtins)
import qualified Fregot.Eval.Builtins          as Builtin
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Package        (Package)
import           Fregot.PrettyPrint            ((<+>))
import qualified Fregot.PrettyPrint            as PP
import           Fregot.Sources.SourceSpan     (SourceSpan)
import           Fregot.TypeCheck.Types        (RuleType, Type)
import qualified Fregot.TypeCheck.Types        as Types

type SourceType = (Type, NonEmpty SourceSpan)

data TypeError
    = UnboundVars (HMS.HashMap UnqualifiedVar SourceSpan)
    | NoUnify (Maybe SourceSpan) SourceType SourceType
    | ArityMismatch SourceSpan Function Int Int
    | InternalError String

data InferEnv = InferEnv
    { _ieBuiltins     :: Builtins Proxy
    , _ieDependencies :: HMS.HashMap PackageName (Package RuleType)
    , _ieThisPackage  :: PackageName
    }

type InferState = Unify.Unification UnqualifiedVar SourceType

type InferM = ReaderT InferEnv (StateT InferState (Either TypeError))

$(makePrisms ''TypeError)
$(makeLenses ''InferEnv)

instance Unify.MonadUnify UnqualifiedVar SourceType InferM where
    unify             = unifyTypeType
    getUnification    = get
    putUnification    = put
    modifyUnification = modify

fromTypeError :: TypeError -> Error
fromTypeError = \case

    UnboundVars vars -> Error.mkMultiError sub "Unbound variables" $ do
        (v, source) <- HMS.toList vars
        return $ (,) source $
            "The variable" <+> PP.code (PP.pretty v) <+> "is not defined"

    NoUnify mbSource (τ, source NonEmpty.:| _) (σ, _) ->
        Error.mkError sub (fromMaybe source mbSource) "Unification error" $
            "Could not unify type" <+> PP.code (PP.pretty τ) <+>
            "with" <+> PP.code (PP.pretty σ)

    ArityMismatch source name expect got -> Error.mkError sub source
        "Arity mismatch" $
        "The function" <+> PP.code (PP.pretty name) <+> "takes" <+>
        PP.pretty expect <+> "arguments but" <+> PP.pretty got <+> "were given"

    InternalError msg -> Error.mkErrorNoMeta sub $ PP.pretty msg
  where
    sub = "typecheck"

runInfer :: Monad m => InferEnv -> InferM a -> ParachuteT Error m a
runInfer env mx =
    let errOrA = evalStateT (runReaderT mx env) Unify.empty in
    either (fatal . fromTypeError) return errOrA

inferRule :: Rule' -> InferM (Rule Types.RuleType SourceSpan)
inferRule rule = case rule ^. ruleKind of
    FunctionRule arity ->
        -- TODO (jaspervdj): According to the current plan, functions will be
        -- skipped right now and inferred in an "inlined" way.
        pure $ rule & ruleInfo .~ Types.FunctionType arity

    -- TODO(jaspervdj): Here, we need to assign types to the arguments as well
    -- as the return value.  According to the current plan, functions will be
    -- skipped right now and inferred in an "inlined" way.
    --
    -- We'll want to do some concatenation here in addition to the current
    -- traversal.
    CompleteRule -> do
        forOf_ (ruleDefs . traverse) rule inferRuleDefinition
        pure $ rule & ruleInfo .~ Types.CompleteRuleType Types.Any

    GenSetRule -> do
        forOf_ (ruleDefs . traverse) rule inferRuleDefinition
        pure $ rule & ruleInfo .~ Types.GenSetRuleType Types.Any

    GenObjectRule -> do
        forOf_ (ruleDefs . traverse) rule inferRuleDefinition
        pure $ rule & ruleInfo .~ Types.GenObjectRuleType Types.Any Types.Any

inferRuleDefinition :: RuleDefinition SourceSpan -> InferM ()
inferRuleDefinition rdef =
    -- TODO(jaspervdj): Return something that can be merged with other rule
    -- definition infer results.
    --
    -- Here, we'll also want some concatenative step.
    forOf_ (ruleBodies . traverse) rdef inferRuleBody
    -- TODO(jaspervdj): Deal with elses, values, etc.

inferRuleBody :: RuleBody SourceSpan -> InferM ()
inferRuleBody body =
    -- TODO(jaspervdj): propagating the bound variables is really all that
    -- matters here.
    for_ body inferLiteral

inferLiteral
    :: Literal SourceSpan -> InferM ()
inferLiteral lit = do
    -- TODO(jaspervdj): In case we have a negative literal here, what we want to
    -- do is throw away the variables that were bound by it.
    _ <- inferStatement (lit ^. literalStatement)
    return ()
    -- TODO(jaspervdj): infer `with` parts.

inferStatement
    :: Statement SourceSpan -> InferM ()
inferStatement = \case
    TermS t -> () <$ inferTerm t
    AssignS _source _v t -> do
        _tty <- inferTerm t
        error "TODO(jaspervdj): bind type in env"
    UnifyS source l r -> unifyTermTerm source l r

inferTerm
    :: Term SourceSpan -> InferM SourceType

inferTerm (ScalarT source scalar) =
    return $ (, NonEmpty.singleton source) $ inferScalar scalar

inferTerm (CallT source fun args) = do
    builtins <- view ieBuiltins
    case (,) <$> HMS.lookup fun builtins <*> HMS.lookup fun Builtin.wipBuiltinSigs of
        Nothing -> error $ "TODO: not a builtin: " ++ show fun
        Just b  -> inferBuiltin source fun b args

inferTerm (NameT source (LocalName var)) = do
    mbRes <- Unify.lookup var
    case mbRes of
        Nothing -> throwError $ UnboundVars (HMS.singleton var source)
        Just ty -> return ty

inferTerm term@(NameT _source (QualifiedName _pkg _var)) = error $ show $
    "TODO(jaspervdj): Inference for qualified names" <+> PP.pretty' term

inferTerm term = error $ show $
    "TODO(jaspervdj): Inference for" <+> PP.pretty' term

inferScalar :: Scalar -> Type
inferScalar = \case
    String _ -> Types.String
    Number _ -> Types.Number
    Bool   _ -> Types.Boolean
    Null     -> Types.Null

inferBuiltin
    :: SourceSpan
    -> Function -> (Builtin Proxy, Builtin.BuiltinSig) -> [Term SourceSpan]
    -> InferM SourceType
inferBuiltin source name (builtin, sig) args = case checkArity arity args of
    ArityBad got         -> throwError $ ArityMismatch source name arity got
    ArityOk inArgs mbOut -> do
        inTypes <- mapM inferTerm inArgs
        env     <- walk HMS.empty (zip inSig inTypes)
        outTy   <- case outSig of
            Right σ -> return (σ, NonEmpty.singleton source)
            Left  i -> maybe
                (throwError $ InternalError "bad return index")
                return (HMS.lookup i env)
        case mbOut of
            Nothing -> return outTy
            Just o  -> do
                unifyTermType source o outTy
                return (Types.Boolean, NonEmpty.singleton source)
  where
    inSig Builtin.:~> outSig = sig
    arity = Builtin.arity builtin

    walk env []                   = return env
    walk env ((Left i, τ) : more) = case HMS.lookup i env of
        Nothing -> walk (HMS.insert i τ env) more
        Just σ  -> unifyTypeType τ σ >> walk env more
    walk env ((Right σ, τ) : more) =
        unifyTypeType (σ, NonEmpty.singleton source) τ >> walk env more

unifyTermTerm
    :: SourceSpan -> Term SourceSpan -> Term SourceSpan -> InferM ()
unifyTermTerm _ (NameT _ (LocalName α)) (NameT _ (LocalName β)) =
    Unify.bindVar α β

unifyTermTerm source lhs rhs = catching _UnboundVars
    (do
        rhsty <- inferTerm rhs
        unifyTermType source lhs rhsty)
    (\_ -> do
        lhsty <- inferTerm lhs
        unifyTermType source rhs lhsty)

unifyTermType
    :: SourceSpan -> Term SourceSpan -> SourceType -> InferM ()
unifyTermType _source (NameT _ (LocalName α)) σ = Unify.bindTerm α σ
unifyTermType _source term                    σ = do
    τ <- inferTerm term
    unifyTypeType τ σ

unifyTypeType :: SourceType -> SourceType -> InferM ()
unifyTypeType l@(τ, _) r@(σ, _)
    | τ == σ    = return ()
    | otherwise = throwError $ NoUnify Nothing l r
