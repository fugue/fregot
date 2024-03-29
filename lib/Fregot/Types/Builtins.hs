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
    ( TypeRepr (..)
    , unTypeRepr

    , BuiltinChecker (..)
    , BuiltinType (..)
    , out
    , (🡒)
    ) where

import           Data.Kind                    (Type)
import           Fregot.Eval.Value.Conversion (FromVal, ToVal)
import qualified Fregot.Types.Internal        as Ty

data TypeRepr (i :: [Type]) (o :: Type) where
    Out :: ToVal o   => Ty.Type -> TypeRepr '[] o
    In  :: FromVal a => Ty.Type -> TypeRepr i o -> TypeRepr (a ': i) o

-- | Deconstruct a 'TypeRepr' to a list of arguments and the return type.
unTypeRepr :: TypeRepr i o -> ([Ty.Type], Ty.Type)
unTypeRepr (Out ty)   = ([], ty)
unTypeRepr (In ty tr) = let (args, ret) = unTypeRepr tr in (ty : args, ret)

data BuiltinChecker m = BuiltinChecker
    { bcUnify    :: Ty.Type -> Ty.Type -> m Ty.Type
    , bcSubsetOf :: Ty.Type -> Ty.Type -> m ()
    , bcCatch    :: forall a. m a -> m a -> m a
    }

-- | A builtin type has two representations.  One of them is a function that
-- takes a number of types and returns the return type.  This allows us to do
-- highly granular type checking.
--
-- The other representation is a deep embedding.  This can be used to "print"
-- the type or render the capabilities doc.  It is also used to convert
-- arguments to the right shape, so the arities must match.
data BuiltinType (i :: [Type]) (o :: Type) = BuiltinType
    { btCheck :: forall m. Monad m => BuiltinChecker m -> TypeRepr i o -> m Ty.Type
    , btRepr  :: TypeRepr i o
    }

out :: ToVal o => Ty.Type -> BuiltinType '[] o
out ty = BuiltinType
    { btCheck = \_ _ -> pure ty
    , btRepr  = Out ty
    }

(🡒) :: FromVal a => Ty.Type -> BuiltinType i o -> BuiltinType (a ': i) o
(🡒) expect bt = BuiltinType
    { btCheck = \c (In actual t) -> bcSubsetOf c actual expect >> btCheck bt c t
    , btRepr  = In expect (btRepr bt)
    }
infixr 6 🡒
