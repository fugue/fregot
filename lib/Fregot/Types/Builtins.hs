-- Utilities and a very small DSL for typing Builtins.
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE Rank2Types     #-}
{-# LANGUAGE TypeOperators  #-}
module Fregot.Types.Builtins
    ( InTypes (..)
    , BuiltinChecker (..)
    , BuiltinType
    , out
    , (🡒)
    ) where

import qualified Fregot.PrettyPrint    as PP
import           Fregot.Types.Internal

data InTypes (i :: [t]) where
    Nil  :: InTypes '[]
    Cons :: Type -> InTypes i -> InTypes (a ': i)

data BuiltinChecker m = BuiltinChecker
    { bcUnify :: Type -> Type -> m Type
    , bcError :: forall a. PP.SemDoc -> m a
    }

type BuiltinType (i :: [t]) =
    forall m. Monad m => BuiltinChecker m -> InTypes i -> m Type

out :: Type -> BuiltinType '[]
out t = \_ Nil -> pure t

(🡒) :: Type -> BuiltinType i -> BuiltinType (a ': i)
(🡒) expect f = \c (Cons actual t) -> bcUnify c expect actual >> f c t
infixr 6 🡒
