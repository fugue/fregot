{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TypeOperators     #-}
module Fregot.Eval.Builtins
    ( ToVal (..)
    , FromVal (..)

    , Sig (..)

    , Args (..)
    , toArgs

    , BuiltinException (..)
    , Builtin (..)
    , ReadyBuiltin
    , arity

    , Function (..)
    , Builtins
    , defaultBuiltins

    , BuiltinM
    , eitherToBuiltinM
    ) where

import           Control.Applicative          ((<|>))
import           Control.Exception            (Exception, throwIO)
import           Control.Lens                 (preview, review)
import           Control.Monad.Identity       (Identity)
import qualified Data.Aeson                   as A
import           Data.Bifunctor               (first)
import           Data.Char                    (intToDigit)
import           Data.Hashable                (Hashable)
import qualified Data.HashMap.Strict          as HMS
import qualified Data.HashSet                 as HS
import           Data.Int                     (Int64)
import           Data.IORef                   (atomicModifyIORef', newIORef)
import qualified Data.List                    as L
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Time                    as Time
import qualified Data.Time.Clock.POSIX        as Time.POSIX
import qualified Data.Time.RFC3339            as Time.RFC3339
import           Data.Traversable.HigherOrder (HTraversable (..))
import qualified Data.Vector                  as V
import qualified Fregot.Eval.Json             as Json
import           Fregot.Eval.Number           (Number)
import qualified Fregot.Eval.Number           as Number
import           Fregot.Eval.Value
import           Fregot.Names
import           Fregot.Prepare.Ast           (BinOp (..), Function (..))
import           Fregot.TypeCheck.Builtins    ((.->.))
import qualified Fregot.TypeCheck.Builtins    as Ty
import qualified Fregot.TypeCheck.Types       as Ty
import           Numeric                      (showIntAtBase)
import qualified Text.Pcre2                   as Pcre2
import qualified Text.Printf.Extended         as Printf
import           Text.Read                    (readMaybe)

class ToVal a where
    toVal :: a -> Value

instance ToVal Value where
    toVal = id

instance ToVal T.Text where
    toVal = StringV

instance ToVal Number where
    toVal = NumberV

instance ToVal Int where
    toVal = toVal . (fromIntegral :: Int -> Int64)

instance ToVal Int64 where
    toVal = toVal . review Number.int

instance ToVal Double where
    toVal = toVal . review Number.double

instance ToVal Bool where
    toVal = BoolV

instance ToVal a => ToVal (V.Vector a) where
    toVal = ArrayV . fmap toVal

instance ToVal a => ToVal [a] where
    toVal = toVal . V.fromList

instance ToVal a => ToVal (HS.HashSet a) where
    toVal = SetV . HS.map toVal

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
    fromVal = fmap (fromIntegral :: Int64 -> Int) . fromVal

instance FromVal Int64 where
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

instance (Eq a, FromVal a, Hashable a) => FromVal (HS.HashSet a)  where
    fromVal (SetV s) = fmap HS.fromList $ traverse fromVal (HS.toList s)
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
    Out :: ToVal o   => Sig '[] o

data Args (a :: [t]) where
    Nil  :: Args '[]
    Cons :: a -> Args as -> Args (a ': as)

-- | TODO (jaspervdj): Use arity check instead?
toArgs :: Sig t o -> [Value] -> Either String (Args t, Maybe Value)
toArgs Out      []       = return (Nil, Nothing)
toArgs Out      [final]  = return (Nil, Just final)
toArgs Out      _        = Left "too many arguments supplied"
toArgs (In _)   []       = Left "not enough arguments supplied"
toArgs (In sig) (x : xs) = do
    arg           <- fromVal x
    (args, final) <- toArgs sig xs
    return (Cons arg args, final)

data BuiltinException = BuiltinException String deriving (Show)

instance Exception BuiltinException

type BuiltinM a = IO a

eitherToBuiltinM :: Either String a -> BuiltinM a
eitherToBuiltinM = either throwBuiltinException return

throwBuiltinException :: String -> BuiltinM a
throwBuiltinException = throwIO . BuiltinException

-- | A builtin function and its signature.
data Builtin m where
    -- TODO(jaspervdj): BuiltinType and Sig are somewhat redundant.
    Builtin
        :: ToVal o
        => Sig i o -> Ty.BuiltinType i -> m (Args i -> BuiltinM o) -> Builtin m

instance HTraversable Builtin where
    htraverse f (Builtin sig ty impl) = Builtin sig ty <$> f impl

type ReadyBuiltin = Builtin Identity

arity :: Builtin m -> Int
arity (Builtin sig _ _) = go 0 sig
  where
    go :: Int -> Sig i o -> Int
    go !acc Out    = acc
    go !acc (In s) = go (acc + 1) s

type Builtins m = HMS.HashMap Function (Builtin m)

defaultBuiltins :: Builtins IO
defaultBuiltins = HMS.fromList
    [ (NamedFunction (BuiltinName "all"),                       builtin_all)
    , (NamedFunction (BuiltinName "any"),                       builtin_any)
    , (NamedFunction (QualifiedName "array" "concat"),          builtin_array_concat)
    , (NamedFunction (BuiltinName "and"),                       builtin_bin_and)
    , (NamedFunction (BuiltinName "concat"),                    builtin_concat)
    , (NamedFunction (BuiltinName "contains"),                  builtin_contains)
    , (NamedFunction (BuiltinName "count"),                     builtin_count)
    , (NamedFunction (BuiltinName "endswith"),                  builtin_endswith)
    , (NamedFunction (BuiltinName "format_int"),                builtin_format_int)
    , (NamedFunction (BuiltinName "indexof"),                   builtin_indexof)
    , (NamedFunction (BuiltinName "intersection"),              builtin_intersection)
    , (NamedFunction (BuiltinName "is_array"),                  builtin_is_array)
    , (NamedFunction (BuiltinName "is_object"),                 builtin_is_object)
    , (NamedFunction (BuiltinName "is_string"),                 builtin_is_string)
    , (NamedFunction (QualifiedName "json" "unmarshal"),        builtin_json_unmarshal)
    , (NamedFunction (BuiltinName "lower"),                     builtin_lower)
    , (NamedFunction (BuiltinName "max"),                       builtin_max)
    , (NamedFunction (BuiltinName "min"),                       builtin_min)
    , (NamedFunction (BuiltinName "or"),                        builtin_bin_or)
    , (NamedFunction (BuiltinName "product"),                   builtin_product)
    , (NamedFunction (BuiltinName "re_match"),                  builtin_re_match)
    , (NamedFunction (BuiltinName "replace"),                   builtin_replace)
    , (NamedFunction (BuiltinName "set"),                       builtin_set)
    , (NamedFunction (BuiltinName "sort"),                      builtin_sort)
    , (NamedFunction (BuiltinName "split"),                     builtin_split)
    , (NamedFunction (BuiltinName "sprintf"),                   builtin_sprintf)
    , (NamedFunction (BuiltinName "substring"),                 builtin_substring)
    , (NamedFunction (BuiltinName "sum"),                       builtin_sum)
    , (NamedFunction (BuiltinName "startswith"),                builtin_startswith)
    , (NamedFunction (BuiltinName "to_number"),                 builtin_to_number)
    , (NamedFunction (QualifiedName "time" "now_ns"),           builtin_time_now_ns)
    , (NamedFunction (QualifiedName "time" "date"),             builtin_time_date)
    , (NamedFunction (QualifiedName "time" "parse_rfc3339_ns"), builtin_time_parse_rfc3339_ns)
    , (NamedFunction (BuiltinName "trim"),                      builtin_trim)
    , (NamedFunction (BuiltinName "upper"),                     builtin_upper)
    , (NamedFunction (BuiltinName "union"),                     builtin_union)
    , (OperatorFunction BinAndO,             builtin_bin_and)
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
    , (OperatorFunction ModuloO,             builtin_modulo)
    , (OperatorFunction BinOrO,              builtin_bin_or)
    ]

builtin_all :: Monad m => Builtin m
builtin_all = Builtin
    (In Out)
    (Ty.collectionOf Ty.Boolean .->. Ty.out Ty.Boolean) $ pure $
    \(Cons arg Nil) -> case arg of
        InL arr -> return $! all (== BoolV True) (arr :: V.Vector Value)
        InR set -> return $! all (== BoolV True) $ HS.toList set

builtin_any :: Monad m => Builtin m
builtin_any = Builtin
    (In Out)
    (Ty.collectionOf Ty.Boolean .->. Ty.out Ty.Boolean) $ pure $
    \(Cons arg Nil) -> case arg of
        InL arr -> return $! any (== BoolV True) (arr :: V.Vector Value)
        InR set -> return $! HS.member (BoolV True) set

builtin_array_concat :: Monad m => Builtin m
builtin_array_concat = Builtin
    (In (In Out))
    -- TODO(jaspervdj): We want `∀a b. array<a> -> array<b> -> array<a|b>`.
    (Ty.Array Ty.Any .->. Ty.Array Ty.Any .->. Ty.out Ty.Any) $ pure $
    \(Cons l (Cons r Nil)) -> return (l <> r :: V.Vector Value)

builtin_concat :: Monad m => Builtin m
builtin_concat = Builtin
    (In (In Out))
    (Ty.String .->. Ty.collectionOf Ty.String .->. Ty.out Ty.String) $ pure $
    \(Cons delim (Cons (Collection texts) Nil)) ->
    return $! T.intercalate delim texts

builtin_contains :: Monad m => Builtin m
builtin_contains = Builtin
    (In (In Out))
    (Ty.String .->. Ty.String .->. Ty.out Ty.Boolean) $ pure $
    \(Cons str (Cons search Nil)) -> return $! search `T.isInfixOf` str

builtin_count :: Monad m => Builtin m
builtin_count = Builtin
    (In Out)
    (Ty.Or (Ty.collectionOf Ty.Any) Ty.String .->. Ty.out Ty.Number) $ pure $
    \(Cons countee Nil) -> case countee of
        InL (Collection c) -> return $! length (c :: [Value])
        InR txt            -> return $! T.length txt

builtin_endswith :: Monad m => Builtin m
builtin_endswith = Builtin
    (In (In Out))
    (Ty.String .->. Ty.String .->. Ty.out Ty.Boolean) $ pure $
    \(Cons str (Cons suffix Nil)) -> return $! suffix `T.isSuffixOf` str

builtin_format_int :: Monad m => Builtin m
builtin_format_int = Builtin
    (In (In Out))
    (Ty.Number .->. Ty.Number .->. Ty.out Ty.String) $ pure $
    \(Cons x (Cons base Nil)) ->
    return $! T.pack $ showIntAtBase base intToDigit (x :: Int) ""

builtin_indexof :: Monad m => Builtin m
builtin_indexof = Builtin
    (In (In Out))
    (Ty.String .->. Ty.String .->. Ty.out Ty.Number) $ pure $
    \(Cons haystack (Cons needle Nil)) ->
    let (prefix, match) = T.breakOn needle haystack in
    return $! if
        | T.null needle -> 0
        | T.null match  -> -1
        | otherwise     -> T.length prefix

builtin_intersection :: Monad m => Builtin m
builtin_intersection = Builtin
    (In Out)
    -- TODO(jaspervdj): Maybe this should be `∀a. set<set<a>> -> set<a>`.
    (Ty.Set (Ty.Set Ty.Any) .->. Ty.out (Ty.Set Ty.Any)) $ pure $
    \(Cons set Nil) -> return $! case HS.toList (set :: (HS.HashSet (HS.HashSet Value))) of
      []   -> HS.empty
      sets -> foldr1 HS.intersection sets

builtin_is_array :: Monad m => Builtin m
builtin_is_array = Builtin
    (In Out)
    (Ty.Any .->. Ty.out Ty.Boolean) $ pure $
    \(Cons val Nil) -> case val of
        ArrayV _ -> return True
        _        -> return False

builtin_is_object :: Monad m => Builtin m
builtin_is_object = Builtin
    (In Out)
    (Ty.Any .->. Ty.out Ty.Boolean) $ pure $
    \(Cons val Nil) -> case val of
        ObjectV _ -> return True
        _         -> return False

builtin_is_string :: Monad m => Builtin m
builtin_is_string = Builtin
    (In Out)
    (Ty.Any .->. Ty.out Ty.Boolean) $ pure $
    \(Cons val Nil) -> case val of
        StringV _ -> return True
        _         -> return False

builtin_json_unmarshal :: Monad m => Builtin m
builtin_json_unmarshal = Builtin
    (In Out)
    (Ty.String .->. Ty.out Ty.Any) $ pure $
    \(Cons str Nil) -> case A.eitherDecodeStrict' (T.encodeUtf8 str) of
        Left  err -> throwBuiltinException err
        Right val -> return $! Json.toValue val

builtin_lower :: Monad m => Builtin m
builtin_lower = Builtin
    (In Out)
    (Ty.String .->. Ty.out Ty.String) $ pure $
    \(Cons str Nil) -> return $! T.toLower str

builtin_max :: Monad m => Builtin m
builtin_max = Builtin
    (In Out)
    -- TODO(jaspervdj): More like `∀a. collection<a> -> a`.
    (Ty.collectionOf Ty.Any .->. Ty.out Ty.Any) $ pure $
    \(Cons (Collection vals) Nil) -> return $! case vals of
        [] -> NullV  -- TODO(jaspervdj): Should be undefined.
        _  -> maximum (vals :: [Value])

builtin_min :: Monad m => Builtin m
builtin_min = Builtin
    (In Out)
    -- TODO(jaspervdj): More like `∀a. collection<a> -> a`.
    (Ty.collectionOf Ty.Any .->. Ty.out Ty.Any) $ pure $
    \(Cons (Collection vals) Nil) -> return $! case vals of
        [] -> NullV  -- TODO(jaspervdj): Should be undefined.
        _  -> minimum (vals :: [Value])

builtin_product :: Monad m => Builtin m
builtin_product = Builtin
    (In Out)
    (Ty.collectionOf Ty.Number .->. Ty.out Ty.Number) $ pure $
    \(Cons (Collection vals) Nil) -> return $! num $ product vals

builtin_re_match :: Builtin IO
builtin_re_match = Builtin
    (In (In Out))
    (Ty.String .->. Ty.String .->. Ty.out Ty.Boolean) $ do
    cacheRef <- newIORef HMS.empty
    pure $
        \(Cons pattern (Cons value Nil)) -> do
        errOrRegex <- atomicModifyIORef' cacheRef $ \cache ->
            case HMS.lookup pattern cache of
                Just errOrRegex -> return errOrRegex
                Nothing         ->
                    let errOrRegex = Pcre2.compile pattern in
                    (HMS.insert pattern errOrRegex cache, errOrRegex)

        eitherToBuiltinM $ do
            regex <- first show errOrRegex
            match <- first show (Pcre2.match regex value)
            return $! not $ null match

builtin_replace :: Monad m => Builtin m
builtin_replace = Builtin
    (In (In (In Out)))
    (Ty.String .->. Ty.String .->. Ty.String .->. Ty.out Ty.String) $ pure $
    \(Cons str (Cons old (Cons new Nil))) -> return $! T.replace old new str

utcToNs :: Time.UTCTime -> Int64
utcToNs =
    floor . ((1e9 :: Double) *) . realToFrac . Time.POSIX.utcTimeToPOSIXSeconds

builtin_time_now_ns :: Monad m => Builtin m
builtin_time_now_ns = Builtin
    Out
    (Ty.out Ty.Number) $ pure $
    \Nil -> review Number.int . utcToNs <$> Time.getCurrentTime

builtin_time_date :: Monad m => Builtin m
builtin_time_date = Builtin
    (In Out)
    (Ty.Number .->. Ty.out (Ty.Array Ty.Number)) $ pure $
    \(Cons ns Nil) ->
    let secs      = (fromIntegral $ Number.floor ns) / 1e9
        utc       = Time.POSIX.posixSecondsToUTCTime secs
        (y, m, d) = Time.toGregorian (Time.utctDay utc) in
    return [fromIntegral y, m, d]

builtin_time_parse_rfc3339_ns :: Monad m => Builtin m
builtin_time_parse_rfc3339_ns = Builtin
    (In Out)
    (Ty.String .->. Ty.out Ty.Number) $ pure $
    \(Cons txt Nil) -> case Time.RFC3339.parseTimeRFC3339 txt of
        Just zoned -> return $! utcToNs $ Time.zonedTimeToUTC zoned
        Nothing    -> throwBuiltinException $
            "Could not parse RFC3339 time: " ++ T.unpack txt

builtin_trim :: Monad m => Builtin m
builtin_trim = Builtin
    (In (In Out))
    (Ty.String .->. Ty.String .->. Ty.out Ty.String) $ pure $
    \(Cons str (Cons cutset Nil)) ->
        return $! T.dropAround (\c -> T.any (== c) cutset) str

builtin_upper :: Monad m => Builtin m
builtin_upper = Builtin
    (In Out)
    (Ty.String .->. Ty.out Ty.String) $ pure $
    \(Cons str Nil) -> return $! T.toUpper str

builtin_union :: Monad m => Builtin m
builtin_union = Builtin
    (In Out)
    -- TODO(jaspervdj): Maybe this should be `∀a. set<set<a>> -> set<a>`.
    (Ty.Set (Ty.Set Ty.Any) .->. Ty.out (Ty.Set Ty.Any)) $ pure $
    \(Cons set Nil) ->
        return $! HS.unions $ HS.toList (set :: (HS.HashSet (HS.HashSet Value)))

-- `set()` is OPA's constructor for an empty set, since `{}` is an empty object
builtin_set :: Monad m => Builtin m
builtin_set = Builtin
    Out
    (Ty.out (Ty.Set Ty.Any)) $ pure $
    \Nil -> return $! SetV HS.empty

builtin_sort :: Monad m => Builtin m
builtin_sort = Builtin
    (In Out)
    -- TODO(jaspervdj): Something more akin to `∀a. collection<a> -> array<a>`.
    (Ty.collectionOf Ty.Any .->. Ty.out (Ty.Array Ty.Any)) $ pure $
    \(Cons (Collection vals) Nil) -> return $! L.sort (vals :: [Value])

builtin_split :: Monad m => Builtin m
builtin_split = Builtin
    (In (In Out))
    (Ty.String .->. Ty.String .->. Ty.out (Ty.Array Ty.String)) $ pure $
    \(Cons str (Cons delim Nil)) -> return $! T.splitOn delim str

builtin_sprintf :: Monad m => Builtin m
builtin_sprintf = Builtin
    (In (In Out))
    (Ty.String .->. Ty.Array Ty.Any .->. Ty.out Ty.String) $ pure $
    \(Cons format (Cons args Nil)) -> eitherToBuiltinM $
    fmap T.pack $ Printf.sprintf (T.unpack format) $
    map Printf.Some (args :: [Value])

builtin_substring :: Monad m => Builtin m
builtin_substring = Builtin
    (In (In (In Out)))
    (Ty.String .->. Ty.Number .->. Ty.Number .->. Ty.out Ty.String) $ pure $
    \(Cons str (Cons start (Cons len Nil))) ->
        return $!
        (if len < 0 then id else T.take len) $!
        T.drop start str

builtin_sum :: Monad m => Builtin m
builtin_sum = Builtin
    (In Out)
    (Ty.collectionOf Ty.Number .->. Ty.out Ty.Number) $ pure $
    \(Cons (Collection vals) Nil) -> return $! num $ sum vals

builtin_startswith :: Monad m => Builtin m
builtin_startswith = Builtin
    (In (In Out))
    (Ty.String .->. Ty.String .->. Ty.out Ty.Boolean) $ pure $
    (\(Cons str (Cons prefix Nil)) -> return $! prefix `T.isPrefixOf` str)

builtin_to_number :: Monad m => Builtin m
builtin_to_number = Builtin
    (In Out)
    (Ty.String .->. Ty.out Ty.Number) $ pure $
    \(Cons txt Nil) ->
        let str = T.unpack txt
            mbRead = (Left <$> readMaybe str) <|> (Right <$> readMaybe str) in
        case mbRead of
            Nothing        -> throwBuiltinException $!
                "to_number: couldn't read " ++ str
            Just (Left i)  -> return $ review Number.int i
            Just (Right d) -> return $ review Number.double d

builtin_equal :: Monad m => Builtin m
builtin_equal = Builtin
  (In (In Out))
  (\c (Ty.Cons x (Ty.Cons y Ty.Nil)) -> Ty.bcUnify c x y)
  $ pure $
  -- TODO(jaspervdj): These don't currently work:
  --
  --     0 == 1.2 - 1.2
  --
  -- Because we'll end up comparing an IntV on the left with a DoubleV on the
  -- right.
  \(Cons x (Cons y Nil)) -> return $! BoolV $! x == (y :: Value)

builtin_not_equal :: Monad m => Builtin m
builtin_not_equal = Builtin
  (In (In Out))
  (Ty.Any .->. Ty.Any .->. Ty.out Ty.Boolean) $ pure $
  \(Cons x (Cons y Nil)) -> return $! BoolV $! x /= (y :: Value)

builtin_less_than :: Monad m => Builtin m
builtin_less_than = Builtin
  (In (In Out))
  (Ty.Number .->. Ty.Number .->. Ty.out Ty.Boolean) $ pure $
  \(Cons x (Cons y Nil)) -> return $! x < num y

builtin_less_than_or_equal :: Monad m => Builtin m
builtin_less_than_or_equal = Builtin
  (In (In Out))
  (Ty.Number .->. Ty.Number .->. Ty.out Ty.Boolean) $ pure $
  \(Cons x (Cons y Nil)) -> return $! x <= num y

builtin_greater_than :: Monad m => Builtin m
builtin_greater_than = Builtin
  (In (In Out))
  (Ty.Number .->. Ty.Number .->. Ty.out Ty.Boolean) $ pure $
  \(Cons x (Cons y Nil)) -> return $! x > num y

builtin_greater_than_or_equal :: Monad m => Builtin m
builtin_greater_than_or_equal = Builtin
  (In (In Out))
  (Ty.Number .->. Ty.Number .->. Ty.out Ty.Boolean) $ pure $
  \(Cons x (Cons y Nil)) -> return $! x >= num y

builtin_plus :: Monad m => Builtin m
builtin_plus = Builtin
  (In (In Out))
  (Ty.Number .->. Ty.Number .->. Ty.out Ty.Number) $ pure $
  \(Cons x (Cons y Nil)) -> return $! num $ x + y

builtin_minus :: Monad m => Builtin m
builtin_minus = Builtin
  (In (In Out))
  -- TODO(jaspervdj): Add set difference
  (Ty.Number .->. Ty.Number .->. Ty.out Ty.Number) $ pure $
  \(Cons x (Cons y Nil)) -> case (x, y) of
      (InL x', InL y') -> return $! NumberV $ num $ x' - y'
      (InR x', InR y') -> return $! SetV $ HS.difference (x' :: HS.HashSet Value) y'
      (InL _, InR _) -> throwBuiltinException $ "Expected number but got set"
      (InR _, InL _) -> throwBuiltinException $ "Expected set but got number"

builtin_times :: Monad m => Builtin m
builtin_times = Builtin
  (In (In Out))
  (Ty.Number .->. Ty.Number .->. Ty.out Ty.Number) $ pure $
  \(Cons x (Cons y Nil)) -> return $! num $ x * y

builtin_divide :: Monad m => Builtin m
builtin_divide = Builtin
  (In (In Out))
  (Ty.Number .->. Ty.Number .->. Ty.out Ty.Number) $ pure $
  \(Cons x (Cons y Nil)) -> return $! num $ x / y

builtin_modulo :: Monad m => Builtin m
builtin_modulo = Builtin
  (In (In Out))
  (Ty.Number .->. Ty.Number .->. Ty.out Ty.Number) $ pure $
  \(Cons x (Cons y Nil)) -> return $! x `Number.mod` y

builtin_bin_and :: Monad m => Builtin m
builtin_bin_and = Builtin
  (In (In Out))
  -- TODO(jaspervdj): Maybe this should be `∀a. set<a> -> set<a> -> set<a>`.
  (Ty.Set Ty.Any .->. Ty.Set Ty.Any .->. Ty.out (Ty.Set Ty.Any)) $ pure $
  \(Cons x (Cons y Nil)) -> return $! SetV $ HS.intersection x y

builtin_bin_or :: Monad m => Builtin m
builtin_bin_or = Builtin
  (In (In Out))
  -- TODO(jaspervdj): Maybe this should be `∀a. set<a> -> set<a> -> set<a>`.
  (Ty.Set Ty.Any .->. Ty.Set Ty.Any .->. Ty.out (Ty.Set Ty.Any)) $ pure $
  \(Cons x (Cons y Nil)) -> return $! SetV $ HS.union x y

-- | Auxiliary function to fix types.
num :: Number -> Number
num = id
