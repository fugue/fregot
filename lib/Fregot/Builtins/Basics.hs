-- | Core builtins.
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
module Fregot.Builtins.Basics
    ( builtins
    ) where

import           Control.Applicative      ((<|>))
import           Control.Lens             (ifoldMap, review, (^?))
import           Control.Monad.Trans      (liftIO)
import           Data.Char                (intToDigit, isSpace)
import           Data.Functor             (($>))
import qualified Data.HashMap.Strict      as HMS
import qualified Data.HashSet             as HS
import qualified Data.List                as L
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Data.Vector              as V
import           Fregot.Builtins.Internal
import           Fregot.Eval.Number       (Number)
import qualified Fregot.Eval.Number       as Number
import           Fregot.Eval.Value
import           Fregot.Names
import           Fregot.Prepare.Ast       (BinOp (..))
import           Fregot.Types.Builtins    ((🡒))
import qualified Fregot.Types.Builtins    as Ty
import           Fregot.Types.Internal    ((∪))
import qualified Fregot.Types.Internal    as Ty
import           Numeric                  (showIntAtBase)
import qualified System.IO                as IO
import qualified Text.Printf.Extended     as Printf
import           Text.Read                (readMaybe)

builtins :: Builtins IO
builtins = HMS.fromList
    [ (NamedFunction (BuiltinName "all"),              builtin_all)
    , (NamedFunction (BuiltinName "any"),              builtin_any)
    , (NamedFunction (QualifiedName "array.concat"),   builtin_array_concat)
    , (NamedFunction (QualifiedName "array.slice"),    builtin_array_slice)
    , (NamedFunction (BuiltinName "and"),              builtin_bin_and)
    , (NamedFunction (BuiltinName "concat"),           builtin_concat)
    , (NamedFunction (BuiltinName "contains"),         builtin_contains)
    , (NamedFunction (BuiltinName "count"),            builtin_count)
    , (NamedFunction (BuiltinName "endswith"),         builtin_endswith)
    , (NamedFunction (BuiltinName "format_int"),       builtin_format_int)
    , (NamedFunction (BuiltinName "indexof"),          builtin_indexof)
    , (NamedFunction (BuiltinName "intersection"),     builtin_intersection)
    , (NamedFunction (BuiltinName "is_array"),         builtin_is_array)
    , (NamedFunction (BuiltinName "is_boolean"),       builtin_is_boolean)
    , (NamedFunction (BuiltinName "is_number"),        builtin_is_number)
    , (NamedFunction (BuiltinName "is_object"),        builtin_is_object)
    , (NamedFunction (BuiltinName "is_set"),           builtin_is_set)
    , (NamedFunction (BuiltinName "is_string"),        builtin_is_string)
    , (NamedFunction (BuiltinName "lower"),            builtin_lower)
    , (NamedFunction (BuiltinName "max"),              builtin_max)
    , (NamedFunction (BuiltinName "min"),              builtin_min)
    , (NamedFunction (QualifiedName "object.remove"),  builtin_object_remove)
    , (NamedFunction (BuiltinName "or"),               builtin_bin_or)
    , (NamedFunction (BuiltinName "product"),          builtin_product)
    , (NamedFunction (BuiltinName "replace"),          builtin_replace)
    , (NamedFunction (BuiltinName "set"),              builtin_set)
    , (NamedFunction (BuiltinName "sort"),             builtin_sort)
    , (NamedFunction (BuiltinName "split"),            builtin_split)
    , (NamedFunction (BuiltinName "sprintf"),          builtin_sprintf)
    , (NamedFunction (BuiltinName "substring"),        builtin_substring)
    , (NamedFunction (BuiltinName "sum"),              builtin_sum)
    , (NamedFunction (BuiltinName "startswith"),       builtin_startswith)
    , (NamedFunction (BuiltinName "to_number"),        builtin_to_number)
    , (NamedFunction (BuiltinName "trace"),            builtin_trace)
    , (NamedFunction (BuiltinName "trim"),             builtin_trim)
    , (NamedFunction (BuiltinName "trim_left"),        builtin_trim_left)
    , (NamedFunction (BuiltinName "trim_prefix"),      builtin_trim_prefix)
    , (NamedFunction (BuiltinName "trim_right"),       builtin_trim_right)
    , (NamedFunction (BuiltinName "trim_suffix"),      builtin_trim_suffix)
    , (NamedFunction (BuiltinName "trim_space"),       builtin_trim_space)
    , (NamedFunction (BuiltinName "upper"),            builtin_upper)
    , (NamedFunction (BuiltinName "union"),            builtin_union)
    , (NamedFunction (BuiltinName "walk"),             builtin_walk)
    , (OperatorFunction BinAndO,             builtin_bin_and)
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
    (\tc (Ty.Cons l (Ty.Cons r Ty.Nil)) -> do
        la <- Ty.bcUnify tc l (Ty.arrayOf Ty.any)
        ra <- Ty.bcUnify tc r (Ty.arrayOf Ty.any)
        pure $ fromMaybe (Ty.arrayOf Ty.unknown) $ do
            x <- la ^? Ty.singleton . Ty._Array
            y <- ra ^? Ty.singleton . Ty._Array
            pure . Ty.arrayOf $! x ∪ y) $ pure $
    \(Cons l (Cons r Nil)) -> return (l <> r :: V.Vector Value)

builtin_array_slice :: Monad m => Builtin m
builtin_array_slice = Builtin
    (In (In (In Out)))
    (\tc (Ty.Cons arr (Ty.Cons a (Ty.Cons b Ty.Nil))) -> do
        tarr <- Ty.bcUnify tc arr (Ty.arrayOf Ty.any)
        Ty.bcSubsetOf tc a Ty.number
        Ty.bcSubsetOf tc b Ty.number
        pure . Ty.arrayOf . fromMaybe Ty.unknown $
            tarr ^? Ty.singleton . Ty._Array) $ pure $
    \(Cons arr (Cons a (Cons b Nil))) -> return $! let
        start = max a 0
        end   = min b (V.length arr)
        in case (end-start) of
        n | n > 0 -> V.slice start n arr
        _         -> [] :: V.Vector Value

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
    (\tc (Ty.Cons sets Ty.Nil) -> do
        tsets <- Ty.bcUnify tc sets (Ty.setOf (Ty.setOf Ty.any))
        pure . Ty.setOf . fromMaybe Ty.unknown $
            tsets ^? Ty.singleton . Ty._Set . Ty.singleton . Ty._Set) $ pure $
    \(Cons set Nil) -> return $!
        case HS.toList (set :: (HS.HashSet (HS.HashSet Value))) of
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

builtin_object_remove :: Monad m => Builtin m
builtin_object_remove = Builtin
    (In (In Out))
    (Ty.objectOf Ty.any Ty.any 🡒
     Ty.arrayOf Ty.any ∪ Ty.setOf Ty.any ∪ Ty.objectOf Ty.any Ty.any 🡒
     Ty.out (Ty.objectOf Ty.any Ty.any)) $ pure $
    \(Cons obj (Cons k Nil)) ->
      -- NOTE(sr): We cannot use `Collection`, since it would take the values, not the keys of an object
      let keys = (case unValue k of
                    ObjectV o -> HMS.keys o
                    ArrayV a  -> V.toList a
                    SetV s    -> HS.toList s
                    _          -> []) -- type checker will not let this happen
      in return $! foldr HMS.delete (obj :: HMS.HashMap Value Value) (keys :: [Value])

builtin_product :: Monad m => Builtin m
builtin_product = Builtin
    (In Out)
    (Ty.collectionOf Ty.number 🡒 Ty.out Ty.number) $ pure $
    \(Cons (Collection vals) Nil) -> return $! num $ product vals

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
            Nothing        -> throwString $!
                "to_number: couldn't read " ++ str
            Just (Left i)  -> return $ review Number.int i
            Just (Right d) -> return $ review Number.double d

builtin_trace :: Monad m => Builtin m
builtin_trace = Builtin (In Out)
    (Ty.string 🡒 Ty.out Ty.void) $ pure $
    \(Cons txt Nil) -> liftIO (T.hPutStrLn IO.stderr txt) $> Value NullV

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
      (InL _, InR _) -> throwString $ "Expected number but got set"
      (InR _, InL _) -> throwString $ "Expected set but got number"

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
