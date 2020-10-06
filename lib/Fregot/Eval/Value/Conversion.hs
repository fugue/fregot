{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

Conversion from native Haskell values to and from 'Value'.
-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeOperators     #-}
module Fregot.Eval.Value.Conversion
    ( ToVal (..)
    , FromVal (..)

    -- `ToVal` / `FromVal` helper newtypes.
    , Values (..)
    , Keys (..)
    , (:|:) (..)
    , Json (..)
    ) where

import           Control.Applicative          ((<|>))
import           Control.Arrow                ((>>>))
import           Control.Lens                 (preview, review)
import qualified Data.Aeson                   as Aeson
import           Data.Bifunctor               (first)
import qualified Data.HashMap.Strict          as HMS
import qualified Data.HashSet                 as HS
import           Data.Hashable                (Hashable)
import           Data.Int                     (Int64)
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import qualified Data.Vector                  as V
import qualified Fregot.Eval.Json             as Json
import           Fregot.Eval.Number           (Number)
import qualified Fregot.Eval.Number           as Number
import           Fregot.Eval.Value

class ToVal a where
    toVal :: a -> Value

instance ToVal Value where
    toVal = id

instance ToVal T.Text where
    toVal = Value . StringV

instance ToVal TL.Text where
    toVal = toVal . TL.toStrict

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

instance ToVal a => ToVal (HMS.HashMap Value a) where
    toVal = Value . ObjectV . HMS.map toVal

class FromVal a where
    fromVal :: Value -> Either String a

instance FromVal Value where
    fromVal = Right

instance FromVal (HMS.HashMap Value Value) where
    fromVal (Value (ObjectV o)) = Right o
    fromVal v                   = Left $
        "Expected object but got " ++ describeValue v

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
newtype Values a = Values [a]

-- | Like `Values`, but collects an object's keys, not values.
newtype Keys a = Keys [a]

instance FromVal a => FromVal (Values a) where
    fromVal = unValue >>> \case
        ArrayV  c -> Values <$> traverse fromVal (V.toList c)
        SetV    c -> Values <$> traverse fromVal (HS.toList c)
        ObjectV c -> Values <$> traverse (fromVal . snd) (HMS.toList c)
        v         -> Left $
            "Expected values but got " ++ describeValue (Value v)

instance FromVal a => FromVal (Keys a) where
    fromVal = unValue >>> \case
        ArrayV  c -> Keys <$> traverse fromVal (V.toList c)
        SetV    c -> Keys <$> traverse fromVal (HS.toList c)
        ObjectV c -> Keys <$> traverse (fromVal . fst) (HMS.toList c)
        v         -> Left $
            "Expected keys but got " ++ describeValue (Value v)

-- | Either-like type for when we have weird ad-hoc polymorphism.
data a :|: b = InL a | InR b

instance (ToVal a, ToVal b) => ToVal (a :|: b) where
    toVal (InL x) = toVal x
    toVal (InR y) = toVal y

instance (FromVal a, FromVal b) => FromVal (a :|: b) where
    -- TODO(jaspervdj): We should use a datatype for expected result types, so
    -- we can join them here nicely and not just return the last error message.
    fromVal v = (InL <$> fromVal v) <|> (InR <$> fromVal v)

-- | Convert to and from arguments through an intermediate JSON representation.
newtype Json a = Json {unJson :: a} deriving (Functor)

instance Aeson.ToJSON a => ToVal (Json a) where
    toVal = Json.toValue . Aeson.toJSON . unJson

instance Aeson.FromJSON a => FromVal (Json a) where
    fromVal val = do
        json <- first show $ Json.fromValue val
        case Aeson.fromJSON json of
            Aeson.Error   str -> Left str
            Aeson.Success p   -> Right (Json p)
