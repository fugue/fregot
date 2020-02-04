{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
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
import           Control.Arrow                ((>>>))
import           Control.Lens                 (ifoldMap, preview, review)
import           Control.Monad.Identity       (Identity)
import           Control.Monad.Stream         (Stream)
import           Control.Monad.Stream         as Stream
import           Control.Monad.Trans          (liftIO)
import qualified Data.Aeson                   as A
import           Data.Bifunctor               (first)
import           Data.Char                    (intToDigit, isSpace)
import           Data.Hashable                (Hashable)
import qualified Data.HashMap.Strict          as HMS
import qualified Data.HashSet                 as HS
import           Data.Int                     (Int64)
import           Data.IORef                   (atomicModifyIORef', newIORef)
import           Data.Maybe                   (fromMaybe)
import qualified Data.List                    as L
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Time                    as Time
import qualified Data.Time.Clock.POSIX        as Time.POSIX
import qualified Data.Time.RFC3339            as Time.RFC3339
import           Data.Traversable.HigherOrder (HTraversable (..))
import qualified Data.Vector                  as V
import           Data.Void                    (Void)
import qualified Fregot.Eval.Json             as Json
import           Fregot.Eval.Number           (Number)
import qualified Fregot.Eval.Number           as Number
import           Fregot.Eval.Value
import           Fregot.Names
import           Fregot.Prepare.Ast           (BinOp (..), Function (..))
import           Fregot.Types.Builtins        ((🡒))
import qualified Fregot.Types.Builtins        as Ty
import           Fregot.Types.Internal        ((∪))
import qualified Fregot.Types.Internal        as Ty
import           Numeric                      (showIntAtBase)
import qualified Text.Pcre2                   as Pcre2
import qualified Text.Printf.Extended         as Printf
import           Text.Read                    (readMaybe)

class ToVal a where
    toVal :: a -> Value

instance ToVal Value where
    toVal = id

instance ToVal T.Text where
    toVal = Value . StringV

instance ToVal Number where
    toVal = Value . NumberV

instance ToVal Int where
    toVal = toVal . (fromIntegral :: Int -> Int64)

instance ToVal Int64 where
    toVal = toVal . review Number.int

instance ToVal Double where
    toVal = toVal . review Number.double

instance ToVal Bool where
    toVal = Value . BoolV

instance ToVal a => ToVal (V.Vector a) where
    toVal = Value . ArrayV . fmap toVal

instance ToVal a => ToVal [a] where
    toVal = toVal . V.fromList

instance ToVal a => ToVal (HS.HashSet a) where
    toVal = Value . SetV . HS.map toVal

class FromVal a where
    fromVal :: Value -> Either String a

instance FromVal Value where
    fromVal = Right

instance FromVal T.Text where
    fromVal (Value (StringV t)) = Right t
    fromVal v                   = Left $
        "Expected string but got " ++ describeValue v

instance FromVal Number where
    fromVal (Value (NumberV n)) = Right n
    fromVal v                   = Left $
        "Expected number but got " ++ describeValue v

instance FromVal Int where
    fromVal = fmap (fromIntegral :: Int64 -> Int) . fromVal

instance FromVal Int64 where
    fromVal (Value (NumberV n)) | Just i <- preview Number.int n = Right i
    fromVal v                                                    = Left $
        "Expected int but got " ++ describeValue v

instance FromVal Double where
    fromVal (Value (NumberV n)) | Just d <- preview Number.double n = Right d
    fromVal v                                                       =
        Left $ "Expected double but got " ++ describeValue v

instance FromVal Bool where
    fromVal (Value (BoolV b)) = Right b
    fromVal v                 = Left $
        "Expected bool but got " ++ describeValue v

instance FromVal a => FromVal (V.Vector a) where
    fromVal (Value (ArrayV v)) = traverse fromVal v
    fromVal v                  = Left $
        "Expected array but got " ++ describeValue v

instance FromVal a => FromVal [a] where
    fromVal = fmap V.toList . fromVal

instance (Eq a, FromVal a, Hashable a) => FromVal (HS.HashSet a)  where
    fromVal (Value (SetV s)) = fmap HS.fromList $ traverse fromVal (HS.toList s)
    fromVal v                = Left $ "Expected set but got " ++ describeValue v

-- | Sometimes builtins (e.g. `count`) do not take a specific type, but any
-- sort of collection.
newtype Collection a = Collection [a]

instance FromVal a => FromVal (Collection a) where
    fromVal = unValue >>> \case
        ArrayV  c -> Collection <$> traverse fromVal (V.toList c)
        SetV    c -> Collection <$> traverse fromVal (HS.toList c)
        ObjectV c -> Collection <$> traverse (fromVal . snd) (HMS.toList c)
        v         -> Left $
            "Expected collection but got " ++ describeValue (Value v)

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
toArgs :: Sig t o -> [Value] -> Either String (Args t)
toArgs Out      []       = return Nil
toArgs Out      _        = Left "too many arguments supplied"
toArgs (In _)   []       = Left "not enough arguments supplied"
toArgs (In sig) (x : xs) = Cons <$> fromVal x <*> toArgs sig xs

data BuiltinException = BuiltinException String deriving (Show)

type BuiltinM a = Stream BuiltinException Void IO a

eitherToBuiltinM :: Either String a -> BuiltinM a
eitherToBuiltinM = either throwBuiltinException return

throwBuiltinException :: String -> BuiltinM a
throwBuiltinException = Stream.throw . BuiltinException

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
    , (NamedFunction (QualifiedName "array.concat"),            builtin_array_concat)
    , (NamedFunction (BuiltinName "and"),                       builtin_bin_and)
    , (NamedFunction (BuiltinName "concat"),                    builtin_concat)
    , (NamedFunction (BuiltinName "contains"),                  builtin_contains)
    , (NamedFunction (BuiltinName "count"),                     builtin_count)
    , (NamedFunction (BuiltinName "endswith"),                  builtin_endswith)
    , (NamedFunction (BuiltinName "format_int"),                builtin_format_int)
    , (NamedFunction (BuiltinName "indexof"),                   builtin_indexof)
    , (NamedFunction (BuiltinName "intersection"),              builtin_intersection)
    , (NamedFunction (BuiltinName "is_array"),                  builtin_is_array)
    , (NamedFunction (BuiltinName "is_boolean"),                builtin_is_boolean)
    , (NamedFunction (BuiltinName "is_number"),                 builtin_is_number)
    , (NamedFunction (BuiltinName "is_object"),                 builtin_is_object)
    , (NamedFunction (BuiltinName "is_set"),                    builtin_is_set)
    , (NamedFunction (BuiltinName "is_string"),                 builtin_is_string)
    , (NamedFunction (QualifiedName "json.unmarshal"),          builtin_json_unmarshal)
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
    , (NamedFunction (QualifiedName "time.now_ns"),             builtin_time_now_ns)
    , (NamedFunction (QualifiedName "time.date"),               builtin_time_date)
    , (NamedFunction (QualifiedName "time.parse_rfc3339_ns"),   builtin_time_parse_rfc3339_ns)
    , (NamedFunction (BuiltinName "trim"),                      builtin_trim)
    , (NamedFunction (BuiltinName "trim_left"),                 builtin_trim_left)
    , (NamedFunction (BuiltinName "trim_prefix"),               builtin_trim_prefix)
    , (NamedFunction (BuiltinName "trim_right"),                builtin_trim_right)
    , (NamedFunction (BuiltinName "trim_suffix"),               builtin_trim_suffix)
    , (NamedFunction (BuiltinName "trim_space"),                builtin_trim_space)
    , (NamedFunction (BuiltinName "upper"),                     builtin_upper)
    , (NamedFunction (BuiltinName "union"),                     builtin_union)
    , (NamedFunction (BuiltinName "walk"),                      builtin_walk)
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
    (Ty.collectionOf Ty.boolean 🡒 Ty.out Ty.boolean) $ pure $
    \(Cons arg Nil) -> case arg of
        InL arr -> return $! all (== Value (BoolV True)) (arr :: V.Vector Value)
        InR set -> return $! all (== Value (BoolV True)) $ HS.toList set

builtin_any :: Monad m => Builtin m
builtin_any = Builtin
    (In Out)
    (Ty.collectionOf Ty.boolean 🡒 Ty.out Ty.boolean) $ pure $
    \(Cons arg Nil) -> case arg of
        InL arr -> return $! any (== Value (BoolV True)) (arr :: V.Vector Value)
        InR set -> return $! HS.member (Value (BoolV True)) set

builtin_array_concat :: Monad m => Builtin m
builtin_array_concat = Builtin
    (In (In Out))
    -- TODO(jaspervdj): We want `∀a b. array<a> -> array<b> -> array<a|b>`.
    (Ty.arrayOf Ty.any 🡒 Ty.arrayOf Ty.any 🡒 Ty.out Ty.unknown) $ pure $
    \(Cons l (Cons r Nil)) -> return (l <> r :: V.Vector Value)

builtin_concat :: Monad m => Builtin m
builtin_concat = Builtin
    (In (In Out))
    (Ty.string 🡒 Ty.collectionOf Ty.string 🡒 Ty.out Ty.string) $ pure $
    \(Cons delim (Cons (Collection texts) Nil)) ->
    return $! T.intercalate delim texts

builtin_contains :: Monad m => Builtin m
builtin_contains = Builtin
    (In (In Out))
    (Ty.string 🡒 Ty.string 🡒 Ty.out Ty.boolean) $ pure $
    \(Cons str (Cons search Nil)) -> return $! search `T.isInfixOf` str

builtin_count :: Monad m => Builtin m
builtin_count = Builtin
    (In Out)
    (Ty.collectionOf Ty.any ∪ Ty.string 🡒 Ty.out Ty.number) $ pure $
    \(Cons countee Nil) -> case countee of
        InL (Collection c) -> return $! length (c :: [Value])
        InR txt            -> return $! T.length txt

builtin_endswith :: Monad m => Builtin m
builtin_endswith = Builtin
    (In (In Out))
    (Ty.string 🡒 Ty.string 🡒 Ty.out Ty.boolean) $ pure $
    \(Cons str (Cons suffix Nil)) -> return $! suffix `T.isSuffixOf` str

builtin_format_int :: Monad m => Builtin m
builtin_format_int = Builtin
    (In (In Out))
    (Ty.number 🡒 Ty.number 🡒 Ty.out Ty.string) $ pure $
    \(Cons x (Cons base Nil)) ->
    return $! T.pack $ showIntAtBase base intToDigit (x :: Int) ""

builtin_indexof :: Monad m => Builtin m
builtin_indexof = Builtin
    (In (In Out))
    (Ty.string 🡒 Ty.string 🡒 Ty.out Ty.number) $ pure $
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
    (Ty.setOf (Ty.setOf Ty.any) 🡒 Ty.out (Ty.setOf Ty.unknown)) $ pure $
    \(Cons set Nil) -> return $! case HS.toList (set :: (HS.HashSet (HS.HashSet Value))) of
      []   -> HS.empty
      sets -> foldr1 HS.intersection sets

builtin_is_array :: Monad m => Builtin m
builtin_is_array = Builtin
    (In Out)
    (Ty.any 🡒 Ty.out Ty.boolean) $ pure $
    \(Cons val Nil) -> case unValue val of
        ArrayV _ -> return True
        _        -> return False

builtin_is_boolean :: Monad m => Builtin m
builtin_is_boolean = Builtin
    (In Out)
    (Ty.any 🡒 Ty.out Ty.boolean) $ pure $
    \(Cons val Nil) -> case unValue val of
        BoolV _ -> return True
        _       -> return False

builtin_is_number :: Monad m => Builtin m
builtin_is_number = Builtin
    (In Out)
    (Ty.any 🡒 Ty.out Ty.boolean) $ pure $
    \(Cons val Nil) -> case unValue val of
        NumberV _ -> return True
        _         -> return False

builtin_is_object :: Monad m => Builtin m
builtin_is_object = Builtin
    (In Out)
    (Ty.any 🡒 Ty.out Ty.boolean) $ pure $
    \(Cons val Nil) -> case unValue val of
        ObjectV _ -> return True
        _         -> return False

builtin_is_set :: Monad m => Builtin m
builtin_is_set = Builtin
    (In Out)
    (Ty.any 🡒 Ty.out Ty.boolean) $ pure $
    \(Cons val Nil) -> case unValue val of
        SetV _ -> return True
        _      -> return False

builtin_is_string :: Monad m => Builtin m
builtin_is_string = Builtin
    (In Out)
    (Ty.any 🡒 Ty.out Ty.boolean) $ pure $
    \(Cons val Nil) -> case unValue val of
        StringV _ -> return True
        _         -> return False

builtin_json_unmarshal :: Monad m => Builtin m
builtin_json_unmarshal = Builtin
    (In Out)
    (Ty.string 🡒 Ty.out Ty.unknown) $ pure $
    \(Cons str Nil) -> case A.eitherDecodeStrict' (T.encodeUtf8 str) of
        Left  err -> throwBuiltinException err
        Right val -> return $! Json.toValue val

builtin_lower :: Monad m => Builtin m
builtin_lower = Builtin
    (In Out)
    (Ty.string 🡒 Ty.out Ty.string) $ pure $
    \(Cons str Nil) -> return $! T.toLower str

builtin_max :: Monad m => Builtin m
builtin_max = Builtin
    (In Out)
    -- TODO(jaspervdj): More like `∀a. collection<a> -> a`.
    (Ty.collectionOf Ty.any 🡒 Ty.out Ty.unknown) $ pure $
    \(Cons (Collection vals) Nil) -> return $! case vals of
        [] -> Value NullV  -- TODO(jaspervdj): Should be undefined.
        _  -> maximum (vals :: [Value])

builtin_min :: Monad m => Builtin m
builtin_min = Builtin
    (In Out)
    -- TODO(jaspervdj): More like `∀a. collection<a> -> a`.
    (Ty.collectionOf Ty.any 🡒 Ty.out Ty.unknown) $ pure $
    \(Cons (Collection vals) Nil) -> return $! case vals of
        [] -> Value NullV  -- TODO(jaspervdj): Should be undefined.
        _  -> minimum (vals :: [Value])

builtin_product :: Monad m => Builtin m
builtin_product = Builtin
    (In Out)
    (Ty.collectionOf Ty.number 🡒 Ty.out Ty.number) $ pure $
    \(Cons (Collection vals) Nil) -> return $! num $ product vals

builtin_re_match :: Builtin IO
builtin_re_match = Builtin
    (In (In Out))
    (Ty.string 🡒 Ty.string 🡒 Ty.out Ty.boolean) $ do
    cacheRef <- newIORef HMS.empty
    pure $
        \(Cons pattern (Cons value Nil)) -> do
        errOrRegex <- liftIO $ atomicModifyIORef' cacheRef $ \cache ->
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
    (Ty.string 🡒 Ty.string 🡒 Ty.string 🡒 Ty.out Ty.string) $ pure $
    \(Cons str (Cons old (Cons new Nil))) -> return $! T.replace old new str

-- `set()` is OPA's constructor for an empty set, since `{}` is an empty object
builtin_set :: Monad m => Builtin m
builtin_set = Builtin
    Out
    (Ty.out (Ty.setOf Ty.unknown)) $ pure $
    \Nil -> return $! Value $ SetV HS.empty

utcToNs :: Time.UTCTime -> Int64
utcToNs =
    floor . ((1e9 :: Double) *) . realToFrac . Time.POSIX.utcTimeToPOSIXSeconds

builtin_time_now_ns :: Monad m => Builtin m
builtin_time_now_ns = Builtin
    Out
    (Ty.out Ty.number) $ pure $
    \Nil -> review Number.int . utcToNs <$> liftIO Time.getCurrentTime

builtin_time_date :: Monad m => Builtin m
builtin_time_date = Builtin
    (In Out)
    (Ty.number 🡒 Ty.out (Ty.arrayOf Ty.number)) $ pure $
    \(Cons ns Nil) ->
    let secs      = (fromIntegral $ Number.floor ns) / 1e9
        utc       = Time.POSIX.posixSecondsToUTCTime secs
        (y, m, d) = Time.toGregorian (Time.utctDay utc) in
    return ([fromIntegral y, m, d] :: [Int])

builtin_time_parse_rfc3339_ns :: Monad m => Builtin m
builtin_time_parse_rfc3339_ns = Builtin
    (In Out)
    (Ty.string 🡒 Ty.out Ty.number) $ pure $
    \(Cons txt Nil) -> case Time.RFC3339.parseTimeRFC3339 txt of
        Just zoned -> return $! utcToNs $ Time.zonedTimeToUTC zoned
        Nothing    -> throwBuiltinException $
            "Could not parse RFC3339 time: " ++ T.unpack txt

builtin_split :: Monad m => Builtin m
builtin_split = Builtin
    (In (In Out))
    (Ty.string 🡒 Ty.string 🡒 Ty.out (Ty.arrayOf Ty.string)) $ pure $
    \(Cons str (Cons delim Nil)) -> return $! T.splitOn delim str

builtin_sprintf :: Monad m => Builtin m
builtin_sprintf = Builtin
    (In (In Out))
    (Ty.string 🡒 Ty.arrayOf Ty.any 🡒 Ty.out Ty.string) $ pure $
    \(Cons format (Cons args Nil)) -> eitherToBuiltinM $
    fmap T.pack $ Printf.sprintf (T.unpack format) $
    map Printf.Some (args :: [Value])

builtin_substring :: Monad m => Builtin m
builtin_substring = Builtin
    (In (In (In Out)))
    (Ty.string 🡒 Ty.number 🡒 Ty.number 🡒 Ty.out Ty.string) $ pure $
    \(Cons str (Cons start (Cons len Nil))) ->
        return $!
        (if len < 0 then id else T.take len) $!
        T.drop start str

builtin_sum :: Monad m => Builtin m
builtin_sum = Builtin
    (In Out)
    (Ty.collectionOf Ty.number 🡒 Ty.out Ty.number) $ pure $
    \(Cons (Collection vals) Nil) -> return $! num $ sum vals

builtin_sort :: Monad m => Builtin m
builtin_sort = Builtin
    (In Out)
    -- TODO(jaspervdj): Something more akin to `∀a. collection<a> -> array<a>`.
    (Ty.collectionOf Ty.any 🡒 Ty.out (Ty.arrayOf Ty.unknown)) $ pure $
    \(Cons (Collection vals) Nil) -> return $! L.sort (vals :: [Value])

builtin_startswith :: Monad m => Builtin m
builtin_startswith = Builtin
    (In (In Out))
    (Ty.string 🡒 Ty.string 🡒 Ty.out Ty.boolean) $ pure $
    (\(Cons str (Cons prefix Nil)) -> return $! prefix `T.isPrefixOf` str)

builtin_to_number :: Monad m => Builtin m
builtin_to_number = Builtin
    (In Out)
    (Ty.string 🡒 Ty.out Ty.number) $ pure $
    \(Cons txt Nil) ->
        let str = T.unpack txt
            mbRead = (Left <$> readMaybe str) <|> (Right <$> readMaybe str) in
        case mbRead of
            Nothing        -> throwBuiltinException $!
                "to_number: couldn't read " ++ str
            Just (Left i)  -> return $ review Number.int i
            Just (Right d) -> return $ review Number.double d

builtin_trim :: Monad m => Builtin m
builtin_trim = Builtin
    (In (In Out))
    (Ty.string 🡒 Ty.string 🡒 Ty.out Ty.string) $ pure $
    \(Cons str (Cons cutset Nil)) ->
        return $! T.dropAround (\c -> T.any (== c) cutset) str

builtin_trim_left :: Monad m => Builtin m
builtin_trim_left = Builtin
    (In (In Out))
    (Ty.string 🡒 Ty.string 🡒 Ty.out Ty.string) $ pure $
    \(Cons str (Cons cutset Nil)) ->
        return $! T.dropWhile (\c -> T.any (== c) cutset) str

builtin_trim_prefix :: Monad m => Builtin m
builtin_trim_prefix = Builtin
    (In (In Out))
    (Ty.string 🡒 Ty.string 🡒 Ty.out Ty.string) $ pure $
    \(Cons str (Cons prefix Nil)) ->
        return $! fromMaybe str $ T.stripPrefix prefix str

builtin_trim_right :: Monad m => Builtin m
builtin_trim_right = Builtin
    (In (In Out))
    (Ty.string 🡒 Ty.string 🡒 Ty.out Ty.string) $ pure $
    \(Cons str (Cons cutset Nil)) ->
        return $! T.dropWhileEnd (\c -> T.any (== c) cutset) str

builtin_trim_suffix :: Monad m => Builtin m
builtin_trim_suffix = Builtin
    (In (In Out))
    (Ty.string 🡒 Ty.string 🡒 Ty.out Ty.string) $ pure $
    \(Cons str (Cons suffix Nil)) ->
        return $! fromMaybe str $ T.stripSuffix suffix str

builtin_trim_space :: Monad m => Builtin m
builtin_trim_space = Builtin
    (In Out)
    (Ty.string 🡒 Ty.out Ty.string) $ pure $
    \(Cons str Nil) -> return $! T.dropAround isSpace str

builtin_upper :: Monad m => Builtin m
builtin_upper = Builtin
    (In Out)
    (Ty.string 🡒 Ty.out Ty.string) $ pure $
    \(Cons str Nil) -> return $! T.toUpper str

builtin_union :: Monad m => Builtin m
builtin_union = Builtin
    (In Out)
    -- TODO(jaspervdj): Maybe this should be `∀a. set<set<a>> -> set<a>`.
    (Ty.setOf (Ty.setOf Ty.any) 🡒 Ty.out (Ty.setOf Ty.unknown)) $ pure $
    \(Cons set Nil) ->
        return $! HS.unions $ HS.toList (set :: (HS.HashSet (HS.HashSet Value)))

builtin_walk :: Monad m => Builtin m
builtin_walk = Builtin
    (In Out)
    -- TODO(jaspervdj): We could type this way better if we had proper "pair"
    -- array types.
    (Ty.any 🡒 Ty.out (Ty.arrayOf Ty.unknown)) $ pure $
    \(Cons val Nil) -> walk V.empty val
  where
    walk path val =
        (pure $ Value $ ArrayV [Value (ArrayV path), val]) <>
        (case unValue val of
            ArrayV v  -> ifoldMap (\i -> walk (path <> [toVal i])) v
            SetV   s  -> foldMap (\v -> walk (path <> [v]) v) s
            ObjectV o -> ifoldMap (\k -> walk (path <> [toVal k])) o
            _         -> mempty)

builtin_equal :: Monad m => Builtin m
builtin_equal = Builtin
  (In (In Out))
  (\c (Ty.Cons x (Ty.Cons y Ty.Nil)) -> Ty.bcUnify c x y >> pure Ty.boolean)
  $ pure $
  -- TODO(jaspervdj): These don't currently work:
  --
  --     0 == 1.2 - 1.2
  --
  -- Because we'll end up comparing an IntV on the left with a DoubleV on the
  -- right.
  \(Cons x (Cons y Nil)) -> return $! Value $ BoolV $! x == (y :: Value)

builtin_not_equal :: Monad m => Builtin m
builtin_not_equal = Builtin
  (In (In Out))
  (Ty.any 🡒 Ty.any 🡒 Ty.out Ty.boolean) $ pure $
  \(Cons x (Cons y Nil)) -> return $! Value $ BoolV $! x /= (y :: Value)

builtin_less_than :: Monad m => Builtin m
builtin_less_than = Builtin
  (In (In Out))
  (Ty.number 🡒 Ty.number 🡒 Ty.out Ty.boolean) $ pure $
  \(Cons x (Cons y Nil)) -> return $! x < num y

builtin_less_than_or_equal :: Monad m => Builtin m
builtin_less_than_or_equal = Builtin
  (In (In Out))
  (Ty.number 🡒 Ty.number 🡒 Ty.out Ty.boolean) $ pure $
  \(Cons x (Cons y Nil)) -> return $! x <= num y

builtin_greater_than :: Monad m => Builtin m
builtin_greater_than = Builtin
  (In (In Out))
  (Ty.number 🡒 Ty.number 🡒 Ty.out Ty.boolean) $ pure $
  \(Cons x (Cons y Nil)) -> return $! x > num y

builtin_greater_than_or_equal :: Monad m => Builtin m
builtin_greater_than_or_equal = Builtin
  (In (In Out))
  (Ty.number 🡒 Ty.number 🡒 Ty.out Ty.boolean) $ pure $
  \(Cons x (Cons y Nil)) -> return $! x >= num y

builtin_plus :: Monad m => Builtin m
builtin_plus = Builtin
  (In (In Out))
  (Ty.number 🡒 Ty.number 🡒 Ty.out Ty.number) $ pure $
  \(Cons x (Cons y Nil)) -> return $! num $ x + y

builtin_minus :: Monad m => Builtin m
builtin_minus = Builtin
  (In (In Out))
  -- TODO(jaspervdj): Maybe this should be `∀a. set<a> -> set<a> -> set<a>`.
  (\c (Ty.Cons x (Ty.Cons y Ty.Nil)) ->
    Ty.bcCatch c
        (do
            Ty.bcSubsetOf c x Ty.number
            Ty.bcSubsetOf c y Ty.number
            return Ty.number)
        (do
            Ty.bcSubsetOf c x $ Ty.setOf Ty.any
            Ty.bcSubsetOf c y $ Ty.setOf Ty.any
            return $ Ty.setOf Ty.unknown)) $ pure $
  \(Cons x (Cons y Nil)) -> case (x, y) of
      (InL x', InL y') -> return $! Value $ NumberV $ num $ x' - y'
      (InR x', InR y') -> return $! Value $ SetV $ HS.difference (x' :: HS.HashSet Value) y'
      (InL _, InR _) -> throwBuiltinException $ "Expected number but got set"
      (InR _, InL _) -> throwBuiltinException $ "Expected set but got number"

builtin_times :: Monad m => Builtin m
builtin_times = Builtin
  (In (In Out))
  (Ty.number 🡒 Ty.number 🡒 Ty.out Ty.number) $ pure $
  \(Cons x (Cons y Nil)) -> return $! num $ x * y

builtin_divide :: Monad m => Builtin m
builtin_divide = Builtin
  (In (In Out))
  (Ty.number 🡒 Ty.number 🡒 Ty.out Ty.number) $ pure $
  \(Cons x (Cons y Nil)) -> return $! num $ x / y

builtin_modulo :: Monad m => Builtin m
builtin_modulo = Builtin
  (In (In Out))
  (Ty.number 🡒 Ty.number 🡒 Ty.out Ty.number) $ pure $
  \(Cons x (Cons y Nil)) -> return $! x `Number.mod` y

builtin_bin_and :: Monad m => Builtin m
builtin_bin_and = Builtin
  (In (In Out))
  -- TODO(jaspervdj): Maybe this should be `∀a. set<a> -> set<a> -> set<a>`.
  (Ty.setOf Ty.any 🡒 Ty.setOf Ty.any 🡒 Ty.out (Ty.setOf Ty.unknown)) $ pure $
  \(Cons x (Cons y Nil)) -> return $! Value $ SetV $ HS.intersection x y

builtin_bin_or :: Monad m => Builtin m
builtin_bin_or = Builtin
  (In (In Out))
  -- TODO(jaspervdj): Maybe this should be `∀a. set<a> -> set<a> -> set<a>`.
  (Ty.setOf Ty.any 🡒 Ty.setOf Ty.any 🡒 Ty.out (Ty.setOf Ty.unknown)) $ pure $
  \(Cons x (Cons y Nil)) -> return $! Value $ SetV $ HS.union x y

-- | Auxiliary function to fix types.
num :: Number -> Number
num = id
