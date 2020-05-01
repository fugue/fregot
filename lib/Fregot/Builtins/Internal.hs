-- | Internals that allow you to construct (and run) builtins.
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
module Fregot.Builtins.Internal
    ( ToVal (..)
    , FromVal (..)

    -- `ToVal` / `FromVal` helper newtypes.
    , Collection (..)
    , (:|:) (..)
    , Json (..)

    , Sig (..)

    , Args (..)
    , toArgs

    , BuiltinException (..)
    , Builtin (..)
    , ReadyBuiltin
    , arity

    , Function (..)

    , BuiltinM
    , throwDoc
    , throwString
    , eitherToBuiltinM

    , Builtins
    ) where

import           Control.Applicative          ((<|>))
import           Control.Arrow                ((>>>))
import           Control.Lens                 (preview, review)
import           Control.Monad.Identity       (Identity)
import           Control.Monad.Stream         (Stream)
import           Control.Monad.Stream         as Stream
import qualified Data.Aeson                   as Aeson
import           Data.Bifunctor               (first)
import           Data.Hashable                (Hashable)
import qualified Data.HashMap.Strict          as HMS
import qualified Data.HashSet                 as HS
import           Data.Int                     (Int64)
import qualified Data.Text                    as T
import qualified Data.Text.Lazy               as TL
import           Data.Traversable.HigherOrder (HTraversable (..))
import qualified Data.Vector                  as V
import           Data.Void                    (Void)
import qualified Fregot.Eval.Json             as Json
import           Fregot.Eval.Number           (Number)
import qualified Fregot.Eval.Number           as Number
import           Fregot.Eval.Value
import           Fregot.Prepare.Ast           (Function (..))
import qualified Fregot.PrettyPrint           as PP
import qualified Fregot.Types.Builtins        as Ty

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

-- | Convert to and from arguments through an intermediate JSON representation.
newtype Json a = Json {unJson :: a}

instance Aeson.ToJSON a => ToVal (Json a) where
    toVal = Json.toValue . Aeson.toJSON . unJson

instance Aeson.FromJSON a => FromVal (Json a) where
    fromVal val = do
        json <- first show $ Json.fromValue val
        case Aeson.fromJSON json of
            Aeson.Error   str -> Left str
            Aeson.Success p   -> Right (Json p)

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

data BuiltinException = BuiltinException PP.SemDoc deriving (Show)

type BuiltinM a = Stream BuiltinException Void IO a

eitherToBuiltinM :: Either String a -> BuiltinM a
eitherToBuiltinM = either throwString return

throwString :: String -> BuiltinM a
throwString = throwDoc . PP.pretty

throwDoc :: PP.SemDoc -> BuiltinM a
throwDoc = Stream.throw . BuiltinException

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
