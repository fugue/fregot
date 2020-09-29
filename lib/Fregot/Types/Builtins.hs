{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
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

import           Fregot.Types.Internal

data InTypes (i :: [*]) where
    Nil  :: InTypes '[]
    Cons :: Type -> InTypes i -> InTypes (a ': i)

data BuiltinChecker m = BuiltinChecker
    { bcUnify    :: Type -> Type -> m Type
    , bcSubsetOf :: Type -> Type -> m ()
    , bcCatch    :: forall a. m a -> m a -> m a
    }

type BuiltinType (i :: [*]) =
    forall m. Monad m => BuiltinChecker m -> InTypes i -> m Type

out :: Type -> BuiltinType '[]
out t = \_ Nil -> pure t

(🡒) :: Type -> BuiltinType i -> BuiltinType (a ': i)
(🡒) expect f = \c (Cons actual t) -> bcSubsetOf c actual expect >> f c t
infixr 6 🡒
