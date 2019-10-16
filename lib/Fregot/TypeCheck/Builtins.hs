-- Utilities and a very small DSL for typing Builtins.
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE Rank2Types     #-}
{-# LANGUAGE TypeOperators  #-}
module Fregot.TypeCheck.Builtins
    ( BuiltinCheckerM (..)
    , BuiltinType
    , out
    , (.->.)
    ) where

import qualified Fregot.PrettyPrint     as PP
import           Fregot.TypeCheck.Types

data InTypes (i :: [t]) where
    Nil  :: InTypes '[]
    Cons :: Type -> InTypes i -> InTypes (a ': i)

class Monad m => BuiltinCheckerM m where
    bcUnify :: Type -> Type -> m Type
    bcError :: PP.SemDoc -> m a

type BuiltinType (i :: [t]) =
    forall m. BuiltinCheckerM m => InTypes i -> m Type

out :: Type -> BuiltinType '[]
out t = \Nil -> pure t

(.->.) :: Type -> BuiltinType i -> BuiltinType (a ': i)
(.->.) expect f = \(Cons actual t) -> bcUnify expect actual >> f t
