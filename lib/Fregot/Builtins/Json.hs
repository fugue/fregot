{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

Json-related builtins.
-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Fregot.Builtins.Json
    ( builtins
    ) where

import           Control.Lens             (Traversal', ix, (&), (.~), (<&>),
                                           (^?))
import           Control.Monad            (guard)
import qualified Data.Aeson               as A
import qualified Data.HashMap.Strict      as HMS
import qualified Data.HashSet             as HS
import qualified Data.List.Extended       as List
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Lazy.Encoding  as TL
import qualified Data.Vector              as V
import           Fregot.Builtins.Internal
import qualified Fregot.Eval.Json         as Json
import qualified Fregot.Eval.Number       as Number
import           Fregot.Eval.Value        (Value (..), ValueF (..),
                                           describeValueF, string)
import           Fregot.Names
import qualified Fregot.Sugar             as Sugar
import           Fregot.Types.Builtins    ((🡒))
import qualified Fregot.Types.Builtins    as Ty
import           Fregot.Types.Internal    ((∪))
import qualified Fregot.Types.Internal    as Ty
import           Text.Read                (readMaybe)

builtins :: Builtins IO
builtins = HMS.fromList
    [ (NamedFunction (QualifiedName "json.is_valid"),  builtin_json_is_valid)
    , (NamedFunction (QualifiedName "json.marshal"),   builtin_json_marshal)
    , (NamedFunction (QualifiedName "json.patch"),     builtin_json_patch)
    , (NamedFunction (QualifiedName "json.unmarshal"), builtin_json_unmarshal)
    ]

builtin_json_is_valid :: Monad m => Builtin m
builtin_json_is_valid = Builtin
    (Ty.string 🡒 Ty.out Ty.boolean) $ pure $
    \(Cons str Nil) -> pure $ case A.eitherDecodeStrict' (T.encodeUtf8 str) of
        Left  _              -> False
        Right (_ :: A.Value) -> True

builtin_json_marshal :: Monad m => Builtin m
builtin_json_marshal = Builtin
    (Ty.any 🡒 Ty.out Ty.string) $ pure $
    \(Cons val Nil) -> case Json.fromValue val of
        Left err   -> throwDoc err
        Right json -> return $! TL.decodeUtf8 $! A.encode json

builtin_json_unmarshal :: Monad m => Builtin m
builtin_json_unmarshal = Builtin
    (Ty.string 🡒 Ty.out Ty.unknown) $ pure $
    \(Cons str Nil) -> case A.eitherDecodeStrict' (T.encodeUtf8 str) of
        Left  err -> throwString err
        Right val -> return $! Json.toValue val

type Path = [Value]

data Patch
    = Add Path Value
    | Remove Path
    | Replace Path Value
    | Move Path Path
    | Copy Path Path
    | Test Path Value
    deriving (Show)

parsePath :: Value -> Either String Path
parsePath (Value val) = case val of
    StringV "" -> pure []
    StringV txt -> pure .
        map (string . T.replace "~0" "~" . T.replace "~1" "/") $
        T.split (== '/') $ T.dropWhile (== '/') txt
    ArrayV v -> pure $ V.toList v
    _ -> Left $
        "parsePath: expecting string or array but got " ++ describeValueF val

parsePatch :: Value -> Either String Patch
parsePatch value = do
    obj <- fromVal value
    op <- traverse fromVal $ HMS.lookup (string "op") obj
    let getValue key = case HMS.lookup (string key) obj of
            Nothing -> Left $ "patch missing '" <> T.unpack key <> "' attribute"
            Just v  -> Right v
        getPath key = getValue key >>= parsePath
    case (op :: Maybe T.Text) of
        Just "add"     -> Add <$> getPath "path" <*> getValue "value"
        Just "remove"  -> Remove <$> getPath "path"
        Just "replace" -> Replace <$> getPath "path" <*> getValue "value"
        Just "move"    -> Move <$> getPath "path" <*> getPath "from"
        Just "copy"    -> Copy <$> getPath "path" <*> getPath "from"
        Just "test"    -> Test <$> getPath "path" <*> getValue "value"
        Just x         -> Left $ "unknown patch op: " <> T.unpack x
        Nothing        -> Left "patch missing 'op' attribute"

instance FromVal Patch where
    fromVal = parsePatch

toArrayIndex :: Value -> Maybe Int
toArrayIndex key = case key of
    Value (NumberV n) -> fromIntegral <$> (n ^? Number.int)
    Value (StringV t)
        | Just i <- readMaybe (T.unpack t)
        , t == "0" || not ("0" `T.isPrefixOf` t) -> Just i
    _ -> Nothing

traversePath :: Path -> Traversal' Value Value
traversePath = foldr (\x t -> ixValue x . t) id
  where
    ixValue key f = \case
        Value (ObjectV obj) ->
            Value . ObjectV <$> ix key f obj
        Value (ArrayV arr) | Just i <- toArrayIndex key ->
            Value . ArrayV <$> ix (fromIntegral i) f arr
        Value (SetV set) | HS.member key set ->
            f key <&> \key' -> Value . SetV . HS.insert key' $ HS.delete key set
        val -> pure val

applyPatch :: Patch -> Value -> Maybe Value
applyPatch (Add path value) root = case List.maybeInitLast path of
    Nothing -> Just value
    Just (ks, k) -> do
        -- Parent must exist.
        parent <- root ^? traversePath ks
        case parent of
            Value (ObjectV obj) -> pure $ root
                & traversePath ks .~ Value (ObjectV $ HMS.insert k value obj)
            Value (ArrayV arr) | k == Value (StringV "-") -> pure $ root
                & traversePath ks .~ Value (ArrayV $ V.snoc arr value)
            Value (ArrayV arr)
                | Just i <- toArrayIndex k, i >= 0 && i <= V.length arr ->
                    let (pre, post) = V.splitAt i arr
                        arr' = pre <> V.cons value post in
                    pure $ root & traversePath ks .~ Value (ArrayV arr')
            Value (SetV set) | k == value -> pure $ root
                & traversePath ks .~ Value (SetV $ HS.insert value set)
            _ -> Nothing
applyPatch (Remove path) root = do
    (ks, k) <- List.maybeInitLast path
    parent <- root ^? traversePath ks
    case parent of
        Value (ObjectV obj) | k `HMS.member` obj ->
            pure $ root & traversePath ks .~ Value (ObjectV $ HMS.delete k obj)
        Value (ArrayV arr)
            | Just i <- toArrayIndex k, i >= 0 && i < V.length arr ->
                let (pre, post) = V.splitAt i arr
                    arr' = pre <> V.drop 1 post in
                pure $ root & traversePath ks .~ Value (ArrayV arr')
        Value (SetV set) | k `HS.member` set ->
            pure $ root & traversePath ks .~ Value (SetV $ HS.delete k set)
        _ -> Nothing
applyPatch (Replace [] value) _ = Just value
applyPatch (Replace path value) root =
    applyPatch (Remove path) root >>= applyPatch (Add path value)
applyPatch (Move path from) root = do
    value <- root ^? traversePath from
    applyPatch (Remove from) root >>= applyPatch (Add path value)
applyPatch (Copy path from) root = do
    value <- root ^? traversePath from
    applyPatch (Add path value) root
applyPatch (Test path value) root = do
    actual <- root ^? traversePath path
    guard $ value == actual
    pure root

builtin_json_patch :: Monad m => Builtin m
builtin_json_patch = Builtin
    (Ty.any 🡒 Ty.arrayOf patchTy 🡒 Ty.out Ty.unknown) $ pure $
    \(Cons val (Cons patches Nil)) ->
        case V.foldM (flip applyPatch) val patches of
            Nothing -> mempty
            Just v  -> pure v
  where
    patchTy = Ty.object $ Ty.StaticDynamic
        (HMS.fromList
            [ (Sugar.String "op",   Ty.string)
            , (Sugar.String "path", Ty.string ∪ Ty.arrayOf Ty.any)
            ])
        (Just (Ty.string, Ty.any))
