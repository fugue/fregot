{-|
Copyright   : (c) 2021 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

Regex-related builtins.
-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Builtins.Object
    ( builtins
    ) where

import qualified Data.HashMap.Strict      as HMS
import qualified Data.HashSet             as HS
import           Fregot.Builtins.Internal
import           Fregot.Eval.Value
import           Fregot.Names
import           Fregot.Types.Builtins    ((🡒))
import           Fregot.Types.Internal    ((∪))
import qualified Fregot.Types.Builtins    as Ty
import qualified Fregot.Types.Internal    as Ty

builtins :: Builtins IO
builtins = HMS.fromList
    [ (NamedFunction (QualifiedName "object.filter"),  builtin_object_filter)
    , (NamedFunction (QualifiedName "object.get"),     builtin_object_get)
    , (NamedFunction (QualifiedName "object.remove"),  builtin_object_remove)
    , (NamedFunction (QualifiedName "object.union"),   builtin_object_union)
    ]

builtin_object_filter :: Monad m => Builtin m
builtin_object_filter = Builtin
    (Ty.objectOf Ty.any Ty.any 🡒
     Ty.arrayOf Ty.any ∪ Ty.setOf Ty.any ∪ Ty.objectOf Ty.any Ty.any 🡒
     Ty.out (Ty.objectOf Ty.unknown Ty.unknown)) $ pure $
    \(Cons obj (Cons (Keys keys) Nil)) ->
     return $! HMS.intersection (obj :: HMS.HashMap Value Value) $ HS.toMap $ HS.fromList keys

builtin_object_get :: Monad m => Builtin m
builtin_object_get = Builtin
    (Ty.objectOf Ty.any Ty.any 🡒 Ty.any 🡒 Ty.any 🡒 Ty.out Ty.unknown) $ pure $
    \(Cons obj (Cons key (Cons def Nil))) ->
     return $! HMS.lookupDefault def key (obj :: HMS.HashMap Value Value)

builtin_object_remove :: Monad m => Builtin m
builtin_object_remove = Builtin
    (Ty.objectOf Ty.any Ty.any 🡒
     Ty.arrayOf Ty.any ∪ Ty.setOf Ty.any ∪ Ty.objectOf Ty.any Ty.any 🡒
     Ty.out (Ty.objectOf Ty.unknown Ty.unknown)) $ pure $
    \(Cons obj (Cons (Keys keys) Nil)) ->
      return $! foldr HMS.delete (obj :: HMS.HashMap Value Value) keys

builtin_object_union :: Monad m => Builtin m
builtin_object_union = Builtin
    (Ty.objectOf Ty.any Ty.any 🡒 Ty.objectOf Ty.any Ty.any 🡒
     Ty.out (Ty.objectOf Ty.unknown Ty.unknown)) $ pure $
    \(Cons left (Cons right Nil)) -> pure $! union left right
  where
    union :: Value -> Value -> Value
    union (Value (ObjectV left)) (Value (ObjectV right)) =
        Value . ObjectV $! HMS.unionWith union left right
    union _left right = right
