-- | JWT-related builtins.
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Builtins.Jwt
    ( builtins
    ) where

import           Data.Aeson               as Aeson
import qualified Data.HashMap.Strict      as HMS
import qualified Data.Text                as T
import           Fregot.Builtins.Internal
import           Fregot.Names             hiding (Key)
import           Fregot.Types.Builtins    ((🡒))
import qualified Fregot.Types.Builtins    as Ty
import qualified Fregot.Types.Internal    as Ty
import qualified Web.JWT                  as Jwt

builtins :: Builtins IO
builtins = HMS.fromList
    [ (NamedFunction (QualifiedName "io.jwt.encode_sign"), builtin_encode_sign)
    ]

newtype Headers = Headers Jwt.JOSEHeader
    deriving FromVal via (Json Jwt.JOSEHeader)

newtype Payload = Payload Jwt.JWTClaimsSet
    deriving FromVal via (Json Jwt.JWTClaimsSet)

newtype Key = Key Jwt.Signer
    deriving FromVal via (Json Key)

instance Aeson.FromJSON Key where
    parseJSON = Aeson.withObject "FromJSON Key" $ \obj -> do
        kty <- obj Aeson..: "kty"
        case (kty :: T.Text) of
            "oct" -> do
                k <- obj Aeson..: "k"
                pure . Key $! Jwt.hmacSecret k
            _ -> fail "Expected 'oct' key type"

builtin_encode_sign :: Applicative m => Builtin m
builtin_encode_sign = Builtin
    (In (In (In Out)))
    (Ty.any 🡒 Ty.any 🡒 Ty.any 🡒 Ty.out (Ty.string)) $ pure $
    \(Cons (Headers header) (Cons (Payload payload) (Cons (Key key) Nil))) ->
    let _ = Jwt.encodeSigned key header payload in
    pure $! Jwt.encodeSigned key header mempty
