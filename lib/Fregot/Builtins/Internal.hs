{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

Internals that allow you to construct (and run) builtins.
-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeOperators     #-}
module Fregot.Builtins.Internal
    ( ToVal (..)
    , FromVal (..)

    -- `ToVal` / `FromVal` helper newtypes.
    , Values (..)
    , Keys (..)
    , (:|:) (..)
    , Json (..)

    , Args (..)
    , toArgs

    , BuiltinException (..)
    , Builtin (..)
    , ReadyBuiltin
    , arity

    , Function (..)

    , BuiltinM
    , throwDoc
    , throwString
    , eitherToBuiltinM

    , Builtins
    ) where

import           Control.Monad.Identity       (Identity)
import           Control.Monad.Stream         as Stream
import qualified Data.HashMap.Strict          as HMS
import           Data.Traversable.HigherOrder (HTraversable (..))
import           Data.Void                    (Void)
import           Fregot.Eval.Value
import           Fregot.Eval.Value.Conversion
import           Fregot.Prepare.Ast           (Function (..))
import qualified Fregot.PrettyPrint           as PP
import qualified Fregot.Types.Builtins        as Ty

data Args (a :: [t]) where
    Nil  :: Args '[]
    Cons :: a -> Args as -> Args (a ': as)

-- | TODO (jaspervdj): Use arity check instead?
toArgs :: Ty.TypeRepr t o -> [Value] -> Either String (Args t)
toArgs (Ty.Out _)    []       = return Nil
toArgs (Ty.Out _)    _        = Left "too many arguments supplied"
toArgs (Ty.In _ _)   []       = Left "not enough arguments supplied"
toArgs (Ty.In _ sig) (x : xs) = Cons <$> fromVal x <*> toArgs sig xs

data BuiltinException = BuiltinException PP.SemDoc deriving (Show)

type BuiltinM a = Stream BuiltinException Void IO a

eitherToBuiltinM :: Either String a -> BuiltinM a
eitherToBuiltinM = either throwString return

throwString :: String -> BuiltinM a
throwString = throwDoc . PP.pretty

throwDoc :: PP.SemDoc -> BuiltinM a
throwDoc = Stream.throw . BuiltinException

-- | A builtin function and its signature.
data Builtin m where
    Builtin
        :: ToVal o
        => Ty.BuiltinType i o -> m (Args i -> BuiltinM o) -> Builtin m

instance HTraversable Builtin where
    htraverse f (Builtin ty impl) = Builtin ty <$> f impl

type ReadyBuiltin = Builtin Identity

arity :: Builtin m -> Int
arity (Builtin ty _) = go 0 (Ty.btRepr ty)
  where
    go :: Int -> Ty.TypeRepr i o -> Int
    go !acc (Ty.Out _)  = acc
    go !acc (Ty.In _ s) = go (acc + 1) s

type Builtins m = HMS.HashMap Function (Builtin m)
