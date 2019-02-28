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

import           Control.Monad       (unless)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text           as T
import qualified Data.Text.Read      as TR
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
    Builtin :: ToVal o => Sig i o -> (Args i -> Either String o) -> Builtin

arity :: Builtin -> Int
arity (Builtin sig _) = go 0 sig
  where
    go :: Int -> Sig i o -> Int
    go !acc Out    = acc
    go !acc (In s) = go (acc + 1) s

builtins :: HMS.HashMap [Var] Builtin
builtins = HMS.fromList
    [ (["all"], builtin_all)
    , (["any"], builtin_any)
    , (["count"], builtin_count)
    , (["endswith"], builtin_endswith)
    , (["is_string"], builtin_is_string)
    , (["split"], builtin_split)
    , (["startswith"], builtin_startswith)
    , (["to_number"], builtin_to_number)
    , (["trim"], builtin_trim)
    ]

builtin_all :: Builtin
builtin_all = Builtin (In Out)
    (\(Cons arr Nil) -> return $! V.and (arr :: V.Vector Bool))

builtin_any :: Builtin
builtin_any = Builtin (In Out)
    (\(Cons arr Nil) -> return $! V.or (arr :: V.Vector Bool))

builtin_count :: Builtin
builtin_count = Builtin (In Out)
    (\(Cons arr Nil) -> return $! V.length (arr :: V.Vector Value))

builtin_endswith :: Builtin
builtin_endswith = Builtin (In (In Out))
    (\(Cons str (Cons suffix Nil)) -> return $! suffix `T.isSuffixOf` str)

builtin_is_string :: Builtin
builtin_is_string = Builtin (In Out) $ \(Cons val Nil) -> case val of
    StringV _ -> return True
    _         -> return False

builtin_trim :: Builtin
builtin_trim = Builtin (In (In Out))
    (\(Cons str (Cons cutset Nil)) ->
        return $! T.dropAround (\c -> T.any (== c) cutset) str)

builtin_split :: Builtin
builtin_split = Builtin (In (In Out))
    (\(Cons str (Cons delim Nil)) -> return $! T.splitOn delim str)

builtin_startswith :: Builtin
builtin_startswith = Builtin (In (In Out))
    (\(Cons str (Cons prefix Nil)) -> return $! prefix `T.isPrefixOf` str)

builtin_to_number :: Builtin
builtin_to_number = Builtin (In Out)
    -- TODO(jaspervdj): read floating points
    (\(Cons str Nil) -> do
        (x, remainder) <- TR.decimal str
        unless (T.null remainder) $ Left $
            "to_number: couldn't read " ++ T.unpack str
        return $! NumberV $! fromIntegral (x :: Int))
