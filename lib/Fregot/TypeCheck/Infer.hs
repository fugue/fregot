{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Fregot.TypeCheck.Infer
    ( TypeError (..)

    , InferEnv (..)
    , InferM
    , runInfer

    , inferRule
    ) where

import           Control.Lens                  (forOf_, view, (^.))
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
import           Fregot.Error                  (Error)
import qualified Fregot.Error                  as Error
import           Fregot.Eval.Builtins          (Builtins)
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.PrettyPrint            ((<+>))
import qualified Fregot.PrettyPrint            as PP
import           Fregot.Sources.SourceSpan     (SourceSpan)
import           Fregot.TypeCheck.Types        (Type)
import qualified Fregot.TypeCheck.Types        as Types

type SourceType = (Type, NonEmpty SourceSpan)

data TypeError
    = UnboundVars (HMS.HashMap UnqualifiedVar SourceSpan)
    | NoUnify (Maybe SourceSpan) SourceType SourceType

data InferEnv = InferEnv
    { _ieBuiltins :: Builtins Proxy
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

  where
    sub = "typecheck"

runInfer :: Monad m => InferEnv -> InferM a -> ParachuteT Error m a
runInfer env mx =
    let errOrA = evalStateT (runReaderT mx env) Unify.empty in
    either (fatal . fromTypeError) return errOrA

inferRule :: Rule SourceSpan -> InferM ()
inferRule rule =
    -- TODO(jaspervdj): Here, we need to assign types to the arguments as well
    -- as the return value.  According to the current plan, functions will be
    -- skipped right now and inferred in an "inlined" way.
    --
    -- We'll want to do some concatenation here in addition to the current
    -- traversal.
    forOf_ (ruleDefs . traverse) rule inferRuleDefinition

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

inferTerm (CallT _source fun _args) = do
    builtins <- view ieBuiltins
    case HMS.lookup fun builtins of
        Nothing -> error "TODO: not a builtin"
        Just _  -> error "TODO: Builtin"

inferTerm term = error $ show $
    "TODO(jaspervdj): Inference for" <+> PP.pretty' term

inferScalar :: Scalar -> Type
inferScalar = \case
    String _ -> Types.String
    Number _ -> Types.Number
    Bool   _ -> Types.Boolean
    Null     -> Types.Null

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
unifyTermType _source (NameT _ (LocalName α)) ty = Unify.bindTerm α ty
unifyTermType _source _ _                        = undefined

unifyTypeType :: SourceType -> SourceType -> InferM ()
unifyTypeType l@(τ, _) r@(σ, _)
    | τ == σ    = return ()
    | otherwise = throwError $ NoUnify Nothing l r
