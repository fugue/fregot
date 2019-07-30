{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Fregot.TypeCheck.Infer
    ( InferM (..)
    , inferTerm
    ) where

import           Control.Monad.Parachute   (ParachuteT)
import           Control.Monad.State       (MonadState (..), State, modify)
import           Data.Unification
import           Fregot.Error              (Error)
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Sources.SourceSpan (SourceSpan)
import qualified Fregot.Sugar              as Sugar
import           Fregot.TypeCheck.Types
import qualified Fregot.TypeCheck.Types    as Ty

type Typed = (SourceSpan, Type InstVar)

type Type' = Type InstVar

data Dir = Down Type' | Up

--------------------------------------------------------------------------------

newtype InferM a = InferM
    { unInferM :: ParachuteT Error (State (Unification InstVar Type')) a
    } deriving (Applicative, Functor, Monad, MonadState (Unification InstVar Type'))

instance MonadUnify InstVar (Type InstVar) InferM where
    unify = unifyType

    getUnification    = get
    putUnification    = put
    modifyUnification = modify

--------------------------------------------------------------------------------

inferTerm :: Dir -> Term SourceSpan -> InferM (Type', Term Typed)
inferTerm dir (ScalarT source scalar) = do
    let ty = case scalar of
            Sugar.Number _ -> Ty.Number
            Sugar.String _ -> Ty.String
            Sugar.Bool   _ -> Ty.Boolean
            Sugar.Null     -> Ty.Null

    unifyDir dir ty
    return (ty, ScalarT (source, ty) scalar)

--------------------------------------------------------------------------------

unifyDir :: Dir -> Type' -> InferM ()
unifyDir Up       _ = return ()
unifyDir (Down σ) τ = unifyType σ τ

unifyType :: Type' -> Type' -> InferM ()
unifyType σ τ
    | σ == τ    = return ()
    | otherwise = undefined
