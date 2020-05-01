-- | JWT-related builtins.
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Builtins.Jwt
    ( builtins
    ) where

import           Control.Applicative      ((<|>))
import           Control.Lens             (set, (^.), (^..))
import           Control.Monad.Except     (ExceptT, catchError, runExceptT)
import           Control.Monad.Trans      (liftIO)
import qualified Crypto.JWT               as Jwt
import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Types         as Aeson
import qualified Data.ByteString.Base16   as Base16
import qualified Data.ByteString.Base64   as Base64
import           Data.Functor.Identity    (Identity)
import qualified Data.HashMap.Strict      as HMS
import           Data.Maybe               (isJust)
import qualified Data.Set                 as S
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TL
import           Data.Time                (UTCTime)
import qualified Data.Time                as Time
import           Fregot.Builtins.Internal
import           Fregot.Builtins.Time     (nsToUtc)
import           Fregot.Eval.Value        (emptyObject)
import           Fregot.Names             hiding (Key)
import           Fregot.Types.Builtins    ((🡒))
import qualified Fregot.Types.Builtins    as Ty
import qualified Fregot.Types.Internal    as Ty

builtins :: Builtins IO
builtins = HMS.fromList
    [ (NamedFunction (QualifiedName "io.jwt.encode_sign"),   builtin_encode_sign)
    , (NamedFunction (QualifiedName "io.jwt.decode"),        builtin_decode)
    , (NamedFunction (QualifiedName "io.jwt.decode_verify"), builtin_decode_verify)
    ]

newtype Headers = Headers (Jwt.JWSHeader ())
    deriving FromVal via (Json Headers)
    deriving ToVal via (Json Headers)

instance Aeson.FromJSON Headers where
    parseJSON = Aeson.withObject "FromJSON Headers" $ \obj ->
        Headers <$> Jwt.parseParams (Just obj) Nothing

instance Aeson.ToJSON Headers where
    toJSON (Headers h) = Aeson.object . map snd $! Jwt.params h

newtype Payload = Payload Jwt.ClaimsSet
    deriving FromVal via (Json Jwt.ClaimsSet)
    deriving ToVal via (Json Jwt.ClaimsSet)

newtype Key = Key Jwt.JWK
    deriving FromVal via (Json Jwt.JWK)

runJwsM :: ExceptT Jwt.JWTError IO a -> BuiltinM a
runJwsM mx = liftIO (runExceptT mx) >>=
    either (\e -> throwString $ "JWS error: " ++ show e) pure

builtin_encode_sign :: Applicative m => Builtin m
builtin_encode_sign = Builtin (In (In (In Out)))
    (Ty.any 🡒 Ty.any 🡒 Ty.any 🡒 Ty.out (Ty.string)) $ pure $
    \(Cons (Headers header) (Cons (Payload payload) (Cons (Key key) Nil))) -> do
    signed <- runJwsM $ Jwt.signClaims key header payload
    pure . TL.toStrict . TL.decodeUtf8 $! Jwt.encodeCompact signed

builtin_decode :: Applicative m => Builtin m
builtin_decode = Builtin (In Out)
    (Ty.string 🡒 Ty.out (Ty.arrayOf Ty.unknown)) $ pure $
    \(Cons t Nil) -> do
    -- Decode the compact representation.
    jws <- runJwsM . Jwt.decodeCompact . TL.encodeUtf8 $! TL.fromStrict t
    payload <- getPayload jws
    case jws ^.. Jwt.signatures of
        [sig] ->  pure
            [ toVal . Headers $! sig ^. Jwt.header
            , toVal $! Json payload
            , toVal . T.decodeUtf8 . Base16.encode $! sig ^. Jwt.signature
            ]
        _ -> throwString $
            "JWT tokens with multiple signatures are currently unsupported"
  where
    getPayload :: Jwt.JWS Identity () Jwt.JWSHeader -> BuiltinM Aeson.Value
    getPayload jws
        -- This is a hack.  The `jose` library does not want you to get the
        -- payload of unverified JWS tokens.  Good for them, I guess, but
        -- implementing these builtins requires us to be able to shoot
        -- ourselves in the foot.  We grab the payload from the JSON
        -- representation.
        | Aeson.Object obj <- Aeson.toJSON jws
        , Just (Aeson.String p64) <- HMS.lookup "payload" obj
        , Right p <- Base64.decode $! T.encodeUtf8 p64
        , Right val <- Aeson.eitherDecodeStrict' p = do
            pure val
        | otherwise =
            throwString "Internal error obtaining JWT payload"

-- | Rego represents a number of different concepts as a single "constraints"
-- object.
data Constraints = Constraints
    { _cKeys               :: Jwt.JWKSet
    , _cTime               :: Maybe UTCTime
    , _cValidationSettings :: Jwt.JWTValidationSettings
    } deriving FromVal via (Json Constraints)

parseKey :: Aeson.Object -> Aeson.Parser Jwt.JWKSet
parseKey obj =
    (do
        secret <- obj Aeson..: "secret"
        pure $! Jwt.JWKSet [Jwt.fromOctets $! T.encodeUtf8 secret]) <|>
    (do
        cert <- obj Aeson..: "cert"
        either fail pure $ Aeson.eitherDecodeStrict' (T.encodeUtf8 cert))

parseTime :: Aeson.Object -> Aeson.Parser (Maybe UTCTime)
parseTime obj = fmap (fmap nsToUtc) $! obj Aeson..:? "time"

parseValidationSettings
    :: Aeson.Object -> Aeson.Parser Jwt.JWTValidationSettings
parseValidationSettings obj = do
    alg <- maybe id (\a -> set Jwt.validationSettingsAlgorithms (S.singleton a))
        <$> obj Aeson..:? "alg"
    iss <- maybe id (\i -> set Jwt.jwtValidationSettingsIssuerPredicate (== i))
        <$> obj Aeson..:? "iss"
    aud <- maybe id
            (\a -> set Jwt.jwtValidationSettingsAudiencePredicate (== a))
        <$> obj Aeson..:? "aud"
    pure . alg . iss . aud $! Jwt.defaultJWTValidationSettings (const True)

instance Aeson.FromJSON Constraints where
    parseJSON = Aeson.withObject "FromJSON Constraints" $ \obj -> Constraints
        <$> parseKey obj
        <*> parseTime obj
        <*> parseValidationSettings obj

builtin_decode_verify :: Applicative m => Builtin m
builtin_decode_verify = Builtin (In (In Out))
    (Ty.string 🡒 Ty.any 🡒 Ty.out (Ty.arrayOf Ty.unknown)) $ pure $
    \(Cons t (Cons (Constraints jwk mbTime valSettings) Nil)) -> do
    jws <- runJwsM . Jwt.decodeCompact . TL.encodeUtf8 $! TL.fromStrict t
    time <- liftIO $ maybe Time.getCurrentTime pure mbTime
    mbClaims <- runJwsM $ catchError
        (Just <$> Jwt.verifyClaimsAt valSettings jwk time jws)
        (\_err -> pure Nothing)
    headers <- case jws ^.. Jwt.signatures of
        [sig] -> pure $! sig ^. Jwt.header
        _     -> throwString $
            "JWT tokens with multiple signatures are currently unsupported"
    pure
        [ toVal (isJust mbClaims)
        , toVal $! Headers headers
        , maybe emptyObject (toVal . Payload) mbClaims
        ]
