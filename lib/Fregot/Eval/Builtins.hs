{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}
module Fregot.Eval.Builtins
    (
    ) where

import qualified Data.HashMap.Strict as HMS
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Fregot.Eval.Value

class ToVal a where
    toVal :: a -> Value

instance ToVal T.Text where
    toVal = StringV

instance ToVal a => ToVal [a] where
    toVal = ArrayV . V.fromList . map toVal

class FromVal a where
    fromVal :: Value -> Either String a

instance FromVal T.Text where
    fromVal (StringV t) = Right t
    fromVal v           = Left $ "Expected string but got " ++ describeValue v

instance FromVal a => FromVal [a] where
    fromVal (ArrayV v) = traverse fromVal (V.toList v)
    fromVal v          = Left $ "Expected array but got " ++ describeValue v

data Sig (i :: [t]) (o :: [t]) where
    In  :: FromVal a => Sig i o -> Sig (a ': i) o
    Out :: ToVal a => Sig i o -> Sig i (a ': o)
    End :: Sig '[] '[]

data Values (a :: [t]) where
    Nil  :: Values '[]
    Cons :: a -> Values as -> Values (a ': as)

data Builtin where
    Builtin :: Sig i o -> (Values i -> Values o) -> Builtin

builtins :: HMS.HashMap T.Text Builtin
builtins = HMS.fromList
    [ ("trim", builtin_trim)
    , ("split", builtin_split)
    ]

builtin_trim :: Builtin
builtin_trim = Builtin
    (In (In (Out End)))
    (\(Cons str (Cons cutset Nil)) ->
        Cons (T.dropAround (\c -> T.any (== c) cutset) str) Nil)

builtin_split :: Builtin
builtin_split = Builtin
    (In (In (Out End)))
    (\(Cons str (Cons delim Nil)) -> Cons (T.splitOn delim str) Nil)
