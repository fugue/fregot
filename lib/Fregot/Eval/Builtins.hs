{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
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

    , builtinFunctions
    , builtinOperators
    ) where

import           Control.Applicative ((<|>))
import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet        as HS
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified Data.Vector         as V
import qualified Fregot.Eval.Json    as Json
import           Fregot.Eval.Value
import           Fregot.Prepare.Ast  (BinOp (..))
import           Fregot.Sugar        (Var)
import           Text.Read           (readMaybe)

class ToVal a where
    toVal :: a -> Value

instance ToVal Value where
    toVal = id

instance ToVal T.Text where
    toVal = StringV

instance ToVal Int where
    toVal = IntV

instance ToVal Double where
    toVal = DoubleV

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

instance FromVal Int where
    fromVal (IntV i) =  Right i
    fromVal v        = Left $ "Expected int but got " ++ describeValue v

instance FromVal Double where
    fromVal (DoubleV d) =  Right d
    fromVal v           = Left $ "Expected double but got " ++ describeValue v

instance FromVal Bool where
    fromVal (BoolV b) = Right b
    fromVal v         = Left $ "Expected bool but got " ++ describeValue v

instance FromVal a => FromVal (V.Vector a) where
    fromVal (ArrayV v) = traverse fromVal v
    fromVal v          = Left $ "Expected array but got " ++ describeValue v

instance FromVal a => FromVal [a] where
    fromVal = fmap V.toList . fromVal

instance FromVal (HS.HashSet Value) where
    fromVal (SetV s) = Right s
    fromVal v        = Left $ "Expected set but got " ++ describeValue v

-- | Sometimes builtins (e.g. `count`) do not take a specific type, but any
-- sort of collection.
newtype Collection a = Collection [a]

instance FromVal a => FromVal (Collection a) where
    fromVal (ArrayV  c) = Collection <$> traverse fromVal (V.toList c)
    fromVal (SetV    c) = Collection <$> traverse fromVal (HS.toList c)
    fromVal (ObjectV c) = Collection <$> traverse (fromVal . snd) (V.toList c)
    fromVal v           = Left $
        "Expected collection but got " ++ describeValue v

-- | Either-like type for when we have weird ad-hoc polymorphism.
data a :|: b = InL a | InR b

instance (FromVal a, FromVal b) => FromVal (a :|: b) where
    -- TODO(jaspervdj): We should use a datatype for expected result types, so
    -- we can join them here nicely and not just return the last error message.
    fromVal v = (InL <$> fromVal v) <|> (InR <$> fromVal v)

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

builtinFunctions :: HMS.HashMap [Var] Builtin
builtinFunctions = HMS.fromList
    [ (["all"], builtin_all)
    , (["any"], builtin_any)
    , (["concat"], builtin_concat)
    , (["contains"], builtin_contains)
    , (["count"], builtin_count)
    , (["endswith"], builtin_endswith)
    , (["is_object"], builtin_is_object)
    , (["is_string"], builtin_is_string)
    , (["json", "unmarshal"], builtin_json_unmarshal)
    , (["replace"], builtin_replace)
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

builtin_concat :: Builtin
builtin_concat = Builtin (In (In Out))
    (\(Cons delim (Cons (Collection texts) Nil)) ->
    return $! T.intercalate delim texts)

builtin_contains :: Builtin
builtin_contains = Builtin (In (In Out)) $
    \(Cons str (Cons search Nil)) -> return $! search `T.isInfixOf` str

builtin_count :: Builtin
builtin_count = Builtin (In Out)
    (\(Cons countee Nil) -> case countee of
        InL (Collection c) -> return $! length (c :: [Value])
        InR txt            -> return $! T.length txt)

builtin_endswith :: Builtin
builtin_endswith = Builtin (In (In Out))
    (\(Cons str (Cons suffix Nil)) -> return $! suffix `T.isSuffixOf` str)

builtin_is_object :: Builtin
builtin_is_object = Builtin (In Out) $ \(Cons val Nil) -> case val of
    ObjectV _ -> return True
    _         -> return False

builtin_is_string :: Builtin
builtin_is_string = Builtin (In Out) $ \(Cons val Nil) -> case val of
    StringV _ -> return True
    _         -> return False

builtin_json_unmarshal :: Builtin
builtin_json_unmarshal = Builtin (In Out) $ \(Cons str Nil) -> do
    val <- A.eitherDecodeStrict' (T.encodeUtf8 str)
    return $! Json.toValue val

builtin_replace :: Builtin
builtin_replace = Builtin (In (In (In Out))) $
    \(Cons str (Cons old (Cons new Nil))) -> return $! T.replace old new str

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
    (\(Cons txt Nil) ->
        let str = T.unpack txt
            mbRead = (Left <$> readMaybe str) <|> (Right <$> readMaybe str) in
        case mbRead of
            Nothing        -> Left $! "to_number: couldn't read " ++ str
            Just (Left i)  -> return (IntV i)
            Just (Right d) -> return (DoubleV d))

builtinOperators :: BinOp -> Builtin
builtinOperators = \case
    EqualO -> builtin_equal
    NotEqualO -> builtin_not_equal
    LessThanO -> builtin_less_than
    LessThanOrEqualO -> builtin_less_than_or_equal
    GreaterThanO -> builtin_greater_than
    GreaterThanOrEqualO -> builtin_greater_than_or_equal
    PlusO -> builtin_plus
    MinusO -> builtin_minus
    TimesO -> builtin_times
    DivideO -> builtin_divide
    BinOrO -> builtin_bin_or

builtin_equal :: Builtin
builtin_equal = Builtin (In (In Out))
  -- TODO(jaspervdj): These don't currently work:
  --
  --     0 == 1.2 - 1.2
  --
  -- Because we'll end up comparing an IntV on the left with a DoubleV on the
  -- right.
  (\(Cons x (Cons y Nil)) -> return $! BoolV $! x == (y :: Value))

builtin_not_equal :: Builtin
builtin_not_equal = Builtin (In (In Out))
  (\(Cons x (Cons y Nil)) -> return $! BoolV $! x /= (y :: Value))

builtin_less_than :: Builtin
builtin_less_than = Builtin (In (In Out))
  (\(Cons x (Cons y Nil)) -> return $! numericBinOp (<) (<) x y)

builtin_less_than_or_equal :: Builtin
builtin_less_than_or_equal = Builtin (In (In Out))
  (\(Cons x (Cons y Nil)) -> return $! numericBinOp (<=) (<=) x y)

builtin_greater_than :: Builtin
builtin_greater_than = Builtin (In (In Out))
  (\(Cons x (Cons y Nil)) -> return $! numericBinOp (>) (>) x y)

builtin_greater_than_or_equal :: Builtin
builtin_greater_than_or_equal = Builtin (In (In Out))
  (\(Cons x (Cons y Nil)) -> return $! numericBinOp (>=) (>=) x y)

builtin_plus :: Builtin
builtin_plus = Builtin (In (In Out))
  (\(Cons x (Cons y Nil)) -> return $! numericBinOp (+) (+) x y)

builtin_minus :: Builtin
builtin_minus = Builtin (In (In Out))
  (\(Cons x (Cons y Nil)) -> return $! numericBinOp (-) (-) x y)

builtin_times :: Builtin
builtin_times = Builtin (In (In Out))
  (\(Cons x (Cons y Nil)) -> return $! numericBinOp (*) (*) x y)

builtin_divide :: Builtin
builtin_divide = Builtin (In (In Out))
  (\(Cons x (Cons y Nil)) -> return $!  numericBinOp
    (\i1 i2 -> (fromIntegral i1 :: Double) / fromIntegral i2) (/) x y)

builtin_bin_or :: Builtin
builtin_bin_or = Builtin (In (In Out))
  (\(Cons x (Cons y Nil)) -> return $! SetV $ HS.union x y)

numericBinOp
    :: (ToVal a, ToVal b)
    => (Int -> Int -> a)
    -> (Double -> Double -> b)
    -> (Int :|: Double) -> (Int :|: Double) -> Value
numericBinOp f _ (InL x) (InL y) = toVal $! f x y
numericBinOp _ g (InL x) (InR y) = toVal $! g (fromIntegral x) y
numericBinOp _ g (InR x) (InL y) = toVal $! g x (fromIntegral y)
numericBinOp _ g (InR x) (InR y) = toVal $! g x y
