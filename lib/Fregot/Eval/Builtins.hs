{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}
module Fregot.Eval.Builtins
    ( ToVal (..)
    , FromVal (..)

    , Sig (..)

    , Args (..)
    , toArgs

    , Builtin (..)
    , arity

    , builtins
    ) where

import qualified Data.HashMap.Strict as HMS
import qualified Data.Text           as T
import qualified Data.Vector         as V
import           Fregot.Eval.Value
import           Fregot.Sugar        (Var)

class ToVal a where
    toVal :: a -> Value

instance ToVal Value where
    toVal = id

instance ToVal T.Text where
    toVal = StringV

instance ToVal Int where
    toVal = NumberV . fromIntegral

instance ToVal Bool where
    toVal = BoolV

instance ToVal a => ToVal (V.Vector a) where
    toVal = ArrayV . fmap toVal

instance ToVal a => ToVal [a] where
    toVal = toVal . V.fromList

class FromVal a where
    fromVal :: Value -> Either String a

instance FromVal Value where
    fromVal = Right

instance FromVal T.Text where
    fromVal (StringV t) = Right t
    fromVal v           = Left $ "Expected string but got " ++ describeValue v

instance FromVal Bool where
    fromVal (BoolV b) = Right b
    fromVal v         = Left $ "Expected bool but got " ++ describeValue v

instance FromVal a => FromVal (V.Vector a) where
    fromVal (ArrayV v) = traverse fromVal v
    fromVal v          = Left $ "Expected array but got " ++ describeValue v

instance FromVal a => FromVal [a] where
    fromVal = fmap V.toList . fromVal

data Sig (i :: [t]) (o :: *) where
    In  :: FromVal a => Sig i o -> Sig (a ': i) o
    Out :: ToVal o => Sig '[] o

data Args (a :: [t]) where
    Nil  :: Args '[]
    Cons :: a -> Args as -> Args (a ': as)

toArgs :: Sig t o -> [Value] -> Either String (Args t, Maybe Value)
toArgs Out      []       = return (Nil, Nothing)
toArgs Out      [final]  = return (Nil, Just final)
toArgs Out      _        = Left "too many arguments supplied"
toArgs (In _)   []       = Left "not enough arguments supplied"
toArgs (In sig) (x : xs) = do
    arg           <- fromVal x
    (args, final) <- toArgs sig xs
    return (Cons arg args, final)

data Builtin where
    Builtin :: ToVal o => Sig i o -> (Args i -> o) -> Builtin

arity :: Builtin -> Int
arity (Builtin sig _) = go 0 sig
  where
    go :: Int -> Sig i o -> Int
    go !acc Out    = acc
    go !acc (In s) = go (acc + 1) s

builtins :: HMS.HashMap [Var] Builtin
builtins = HMS.fromList
    [ (["all"], builtin_all)
    , (["count"], builtin_count)
    , (["trim"], builtin_trim)
    , (["split"], builtin_split)
    ]

builtin_all :: Builtin
builtin_all = Builtin
    (In Out)
    (\(Cons arr Nil) -> V.and (arr :: V.Vector Bool))

builtin_count :: Builtin
builtin_count = Builtin
    (In Out)
    (\(Cons arr Nil) -> V.length (arr :: V.Vector Value))

builtin_trim :: Builtin
builtin_trim = Builtin
    (In (In Out))
    (\(Cons str (Cons cutset Nil)) ->
        T.dropAround (\c -> T.any (== c) cutset) str)

builtin_split :: Builtin
builtin_split = Builtin
    (In (In Out))
    (\(Cons str (Cons delim Nil)) -> T.splitOn delim str)
