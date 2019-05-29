module Fregot.TypeCheck.Infer
    ( inferTerm
    ) where

import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Sources.SourceSpan (SourceSpan)
import qualified Fregot.Sugar              as Sugar
import           Fregot.TypeCheck.Types
import qualified Fregot.TypeCheck.Types    as Ty

type Typed = (SourceSpan, Type InstVar)

type Type' = Type InstVar

data Dir = Down Type' | Up

inferTerm :: Monad m => Dir -> Term SourceSpan -> m (Type', Term Typed)
inferTerm dir (ScalarT source scalar) = do
    let ty = case scalar of
            Sugar.Number _ -> Ty.Number
            Sugar.String _ -> Ty.String
            Sugar.Bool   _ -> Ty.Boolean
            Sugar.Null     -> Ty.Null

    unifyDir dir ty
    return (ty, ScalarT (source, ty) scalar)

unifyDir :: Monad m => Dir -> Type' -> m ()
unifyDir Up       _ = return ()
unifyDir (Down σ) τ = unifyType σ τ

unifyType :: Monad m => Type' -> Type' -> m ()
unifyType σ τ
    | σ == τ    = return ()
    | otherwise = undefined
