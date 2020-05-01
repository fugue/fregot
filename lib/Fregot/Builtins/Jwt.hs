-- | JWT-related builtins.
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Builtins.Jwt
    ( builtins
    ) where

import           Control.Monad.Except     (runExceptT)
import           Control.Monad.Trans      (liftIO)
import qualified Crypto.JOSE.Error        as Jose
import qualified Crypto.JWT               as Jwt
import           Data.Aeson               as Aeson
import qualified Data.HashMap.Strict      as HMS
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TL
import           Fregot.Builtins.Internal
import           Fregot.Names             hiding (Key)
import           Fregot.Types.Builtins    ((🡒))
import qualified Fregot.Types.Builtins    as Ty
import qualified Fregot.Types.Internal    as Ty

builtins :: Builtins IO
builtins = HMS.fromList
    [ (NamedFunction (QualifiedName "io.jwt.encode_sign"), builtin_encode_sign)
    ]

newtype Headers = Headers (Jwt.JWSHeader ())
    deriving FromVal via (Json Headers)

instance Aeson.FromJSON Headers where
    parseJSON = Aeson.withObject "FromJSON Headers" $ \obj ->
        Headers <$> Jwt.parseParams (Just obj) Nothing

newtype Payload = Payload Jwt.ClaimsSet
    deriving FromVal via (Json Jwt.ClaimsSet)

newtype Key = Key Jwt.JWK
    deriving FromVal via (Json Jwt.JWK)

builtin_encode_sign :: Applicative m => Builtin m
builtin_encode_sign = Builtin
    (In (In (In Out)))
    (Ty.any 🡒 Ty.any 🡒 Ty.any 🡒 Ty.out (Ty.string)) $ pure $
    \(Cons (Headers header) (Cons (Payload payload) (Cons (Key key) Nil))) -> do
    errOrSigned <- liftIO $ runExceptT $ Jwt.signClaims key header payload
    case errOrSigned of
        Left err -> throwString (show (err :: Jose.Error))
        Right signed ->
            pure . TL.toStrict . TL.decodeUtf8 $! Jwt.encodeCompact signed
