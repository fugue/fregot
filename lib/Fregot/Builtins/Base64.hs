-- | Base64-related builtins.
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Builtins.Base64
    ( builtins
    ) where

import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Base64.URL as Base64.URL
import qualified Data.HashMap.Strict        as HMS
import qualified Data.Text.Encoding         as T
import           Fregot.Builtins.Internal
import qualified Data.ByteString as B
import           Fregot.Names
import           Fregot.Types.Builtins      ((🡒))
import qualified Fregot.Types.Builtins      as Ty
import qualified Fregot.Types.Internal      as Ty

builtins :: Builtins IO
builtins = HMS.fromList
    [ (NamedFunction (QualifiedName "base64.encode"),    encode Base64.encode)
    , (NamedFunction (QualifiedName "base64.decode"),    decode Base64.decode)
    , (NamedFunction (QualifiedName "base64url.encode"), encode Base64.URL.encode)
    , (NamedFunction (QualifiedName "base64url.decode"), decode Base64.URL.decode)
    ]

encode :: Applicative m => (B.ByteString -> B.ByteString) -> Builtin m
encode f = Builtin (In Out)
    (Ty.string 🡒 Ty.out Ty.string) $ pure $
    \(Cons text Nil) -> pure . T.decodeUtf8 . f $! T.encodeUtf8 text

decode
    :: Applicative m
    => (B.ByteString -> Either String B.ByteString) -> Builtin m
decode f = Builtin (In Out)
    (Ty.string 🡒 Ty.out Ty.string) $ pure $
    \(Cons t Nil) -> fmap T.decodeUtf8 . eitherToBuiltinM . f $! T.encodeUtf8 t
