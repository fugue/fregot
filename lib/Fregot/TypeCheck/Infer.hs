{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Fregot.TypeCheck.Infer
    ( InferM
    , runInfer

    , inferRule
    ) where

import           Control.Lens               (forOf_, (^.))
import           Control.Monad.Parachute    (ParachuteT, fatal, mapParachuteT)
import           Control.Monad.State.Strict (State, evalState, get, modify, put)
import           Data.Foldable              (for_)
import qualified Data.Unification           as Unify
import           Fregot.Error               (Error)
import qualified Fregot.Error               as Error
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Sources.SourceSpan  (SourceSpan)
import           Fregot.TypeCheck.Types     (Type)
import qualified Fregot.TypeCheck.Types     as Types

data Ground a
    = Ground a
    | Unground  -- Should we return the variables that are not ground?  YES
    deriving (Functor, Show)

type Env = Unify.Unification UnqualifiedVar Type

type InferM = ParachuteT Error (State Env)

instance Unify.MonadUnify UnqualifiedVar Type InferM where
    unify             = undefined
    getUnification    = get
    putUnification    = put
    modifyUnification = modify

runInfer :: Monad m => InferM a -> ParachuteT Error m a
runInfer = mapParachuteT $ \s -> return $ evalState s Unify.empty

-- | Check that a future value is grounded, or throw an error.
grounded :: SourceSpan -> Ground a -> InferM a
grounded _      (Ground x) = return x
grounded source Unground   = fatal $ Error.mkError
    "typecheck" source "not grounded"
    "This expression contains free variables"

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
    :: Statement SourceSpan -> InferM (Ground ())
inferStatement = \case
    TermS t -> (() <$) <$> inferTerm t
    AssignS source _v t -> do
        _tty <- grounded source =<< inferTerm t
        error "TODO(jaspervdj): bind type in env"
    UnifyS source l r -> (() <$) <$> inferUnify source l r

inferUnify
    :: SourceSpan -> Term SourceSpan -> Term SourceSpan -> InferM (Ground Type)
inferUnify = undefined

inferTerm
    :: Term SourceSpan -> InferM (Ground Type)

inferTerm (ScalarT _ scalar) =
    return $ Ground $ inferScalar scalar

inferTerm _ = undefined

inferScalar :: Scalar -> Type
inferScalar = \case
    String _ -> Types.String
    Number _ -> Types.Number
    Bool   _ -> Types.Boolean
    Null     -> Types.Null
