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

    , builtins
    ) where

import           Control.Applicative ((<|>))
import           Control.Lens        (preview, review)
import qualified Data.Aeson          as A
import           Data.Bifunctor      (first)
import           Data.Char           (intToDigit)
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet        as HS
import qualified Data.List           as L
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified Data.Vector         as V
import qualified Fregot.Eval.Json    as Json
import           Fregot.Eval.Number  (Number)
import qualified Fregot.Eval.Number  as Number
import           Fregot.Eval.Value
import           Fregot.Prepare.Ast  (BinOp (..), Function (..))
import           Numeric             (showIntAtBase)
import qualified Text.Pcre2          as Pcre2
import           Text.Read           (readMaybe)

class ToVal a where
    toVal :: a -> Value

instance ToVal Value where
    toVal = id

instance ToVal T.Text where
    toVal = StringV

instance ToVal Number where
    toVal = NumberV

instance ToVal Int where
    toVal = toVal . review Number.int

instance ToVal Double where
    toVal = toVal . review Number.double

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

instance FromVal Number where
    fromVal (NumberV n) = Right n
    fromVal v           = Left $ "Expected number but got " ++ describeValue v

instance FromVal Int where
    fromVal (NumberV n) | Just i <- preview Number.int n = Right i
    fromVal v           = Left $ "Expected int but got " ++ describeValue v

instance FromVal Double where
    fromVal (NumberV n) | Just d <- preview Number.double n = Right d
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
    fromVal (ObjectV c) = Collection <$> traverse (fromVal . snd) (HMS.toList c)
    fromVal v           = Left $
        "Expected collection but got " ++ describeValue v

-- | Either-like type for when we have weird ad-hoc polymorphism.
data a :|: b = InL a | InR b

instance (ToVal a, ToVal b) => ToVal (a :|: b) where
    toVal (InL x) = toVal x
    toVal (InR y) = toVal y

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

builtins :: HMS.HashMap Function Builtin
builtins = HMS.fromList
    [ (NamedFunction ["all"],                builtin_all)
    , (NamedFunction ["any"],                builtin_any)
    , (NamedFunction ["array", "concat"],    builtin_array_concat)
    , (NamedFunction ["concat"],             builtin_concat)
    , (NamedFunction ["contains"],           builtin_contains)
    , (NamedFunction ["count"],              builtin_count)
    , (NamedFunction ["endswith"],           builtin_endswith)
    , (NamedFunction ["format_int"],         builtin_format_int)
    , (NamedFunction ["is_object"],          builtin_is_object)
    , (NamedFunction ["is_string"],          builtin_is_string)
    , (NamedFunction ["json", "unmarshal"],  builtin_json_unmarshal)
    , (NamedFunction ["max"],                builtin_max)
    , (NamedFunction ["min"],                builtin_min)
    , (NamedFunction ["product"],            builtin_product)
    , (NamedFunction ["re_match"],           builtin_re_match)
    , (NamedFunction ["replace"],            builtin_replace)
    , (NamedFunction ["sort"],               builtin_sort)
    , (NamedFunction ["split"],              builtin_split)
    , (NamedFunction ["sum"],                builtin_sum)
    , (NamedFunction ["startswith"],         builtin_startswith)
    , (NamedFunction ["to_number"],          builtin_to_number)
    , (NamedFunction ["trim"],               builtin_trim)
    , (OperatorFunction EqualO,              builtin_equal)
    , (OperatorFunction NotEqualO,           builtin_not_equal)
    , (OperatorFunction LessThanO,           builtin_less_than)
    , (OperatorFunction LessThanOrEqualO,    builtin_less_than_or_equal)
    , (OperatorFunction GreaterThanO,        builtin_greater_than)
    , (OperatorFunction GreaterThanOrEqualO, builtin_greater_than_or_equal)
    , (OperatorFunction PlusO,               builtin_plus)
    , (OperatorFunction MinusO,              builtin_minus)
    , (OperatorFunction TimesO,              builtin_times)
    , (OperatorFunction DivideO,             builtin_divide)
    , (OperatorFunction BinOrO,              builtin_bin_or)
    ]

builtin_all :: Builtin
builtin_all = Builtin (In Out)
    (\(Cons arr Nil) -> return $! V.and (arr :: V.Vector Bool))

builtin_any :: Builtin
builtin_any = Builtin (In Out)
    (\(Cons arr Nil) -> return $! V.or (arr :: V.Vector Bool))

builtin_array_concat :: Builtin
builtin_array_concat = Builtin (In (In Out)) $
    \(Cons l (Cons r Nil)) -> return (l <> r :: V.Vector Value)

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

builtin_format_int :: Builtin
builtin_format_int = Builtin (In (In Out)) $ \(Cons x (Cons base Nil)) ->
    return $! T.pack $ showIntAtBase base intToDigit (x :: Int) ""

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

builtin_max :: Builtin
builtin_max = Builtin (In Out) $
    \(Cons (Collection vals) Nil) -> return $! case vals of
        [] -> NullV  -- TODO(jaspervdj): Should be undefined.
        _  -> maximum (vals :: [Value])

builtin_min :: Builtin
builtin_min = Builtin (In Out) $
    \(Cons (Collection vals) Nil) -> return $! case vals of
        [] -> NullV  -- TODO(jaspervdj): Should be undefined.
        _  -> minimum (vals :: [Value])

builtin_product :: Builtin
builtin_product = Builtin (In Out) $
    \(Cons (Collection vals) Nil) -> return $! num $ product vals

builtin_re_match :: Builtin
builtin_re_match = Builtin (In (In Out)) $
    \(Cons pattern (Cons value Nil)) -> do
        regex <- first show (Pcre2.compile pattern)
        match <- first show (Pcre2.match regex value)
        return $! not $ null match

builtin_replace :: Builtin
builtin_replace = Builtin (In (In (In Out))) $
    \(Cons str (Cons old (Cons new Nil))) -> return $! T.replace old new str

builtin_trim :: Builtin
builtin_trim = Builtin (In (In Out))
    (\(Cons str (Cons cutset Nil)) ->
        return $! T.dropAround (\c -> T.any (== c) cutset) str)

builtin_sort :: Builtin
builtin_sort = Builtin (In Out) $
    \(Cons (Collection vals) Nil) -> return $! L.sort (vals :: [Value])

builtin_split :: Builtin
builtin_split = Builtin (In (In Out))
    (\(Cons str (Cons delim Nil)) -> return $! T.splitOn delim str)

builtin_sum :: Builtin
builtin_sum = Builtin (In Out) $
    \(Cons (Collection vals) Nil) -> return $! num $ sum vals

builtin_startswith :: Builtin
builtin_startswith = Builtin (In (In Out))
    (\(Cons str (Cons prefix Nil)) -> return $! prefix `T.isPrefixOf` str)

builtin_to_number :: Builtin
builtin_to_number = Builtin (In Out) $
    \(Cons txt Nil) ->
        let str = T.unpack txt
            mbRead = (Left <$> readMaybe str) <|> (Right <$> readMaybe str) in
        case mbRead of
            Nothing        -> Left $! "to_number: couldn't read " ++ str
            Just (Left i)  -> return $ review Number.int i
            Just (Right d) -> return $ review Number.double d

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
builtin_less_than = Builtin (In (In Out)) $
  \(Cons x (Cons y Nil)) -> return $! x < num y

builtin_less_than_or_equal :: Builtin
builtin_less_than_or_equal = Builtin (In (In Out)) $
  \(Cons x (Cons y Nil)) -> return $! x <= num y

builtin_greater_than :: Builtin
builtin_greater_than = Builtin (In (In Out)) $
  \(Cons x (Cons y Nil)) -> return $! x > num y

builtin_greater_than_or_equal :: Builtin
builtin_greater_than_or_equal = Builtin (In (In Out)) $
  \(Cons x (Cons y Nil)) -> return $! x >= num y

builtin_plus :: Builtin
builtin_plus = Builtin (In (In Out)) $
  \(Cons x (Cons y Nil)) -> return $! num $ x + y

builtin_minus :: Builtin
builtin_minus = Builtin (In (In Out)) $
  \(Cons x (Cons y Nil)) -> return $! num $ x - y

builtin_times :: Builtin
builtin_times = Builtin (In (In Out)) $
  \(Cons x (Cons y Nil)) -> return $! num $ x * y

builtin_divide :: Builtin
builtin_divide = Builtin (In (In Out)) $
  \(Cons x (Cons y Nil)) -> return $! num $ x / y

builtin_bin_or :: Builtin
builtin_bin_or = Builtin (In (In Out)) $
  \(Cons x (Cons y Nil)) -> return $! SetV $ HS.union x y

-- | Auxiliary function to fix types.
num :: Number -> Number
num = id
