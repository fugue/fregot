{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

Date- and time-related builtins.
-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Builtins.Time
    ( builtins

    , utcToNs
    , nsToUtc
    ) where

import           Control.Lens             (review)
import           Control.Monad.Trans      (liftIO)
import qualified Data.HashMap.Strict      as HMS
import           Data.Int                 (Int64)
import qualified Data.Text                as T
import qualified Data.Time                as Time
import qualified Data.Time.Clock.POSIX    as Time.POSIX
import qualified Data.Time.RFC3339        as Time.RFC3339
import           Fregot.Builtins.Internal
import qualified Fregot.Eval.Number       as Number
import           Fregot.Names
import           Fregot.Types.Builtins    ((🡒))
import qualified Fregot.Types.Builtins    as Ty
import qualified Fregot.Types.Internal    as Ty

builtins :: Builtins IO
builtins = HMS.fromList
    [ (NamedFunction (QualifiedName "time.now_ns"),           builtin_time_now_ns)
    , (NamedFunction (QualifiedName "time.date"),             builtin_time_date)
    , (NamedFunction (QualifiedName "time.parse_rfc3339_ns"), builtin_time_parse_rfc3339_ns)
    ]

utcToNs :: Time.UTCTime -> Int64
utcToNs =
    floor . ((1e9 :: Double) *) . realToFrac . Time.POSIX.utcTimeToPOSIXSeconds

nsToUtc :: Int64 -> Time.UTCTime
nsToUtc ns =
    let secs = (fromIntegral $ Number.floor $ fromIntegral ns) / 1e9 in
    Time.POSIX.posixSecondsToUTCTime secs

builtin_time_now_ns :: Monad m => Builtin m
builtin_time_now_ns = Builtin
    (Ty.out Ty.number) $ pure $
    \Nil -> review Number.int . utcToNs <$> liftIO Time.getCurrentTime

builtin_time_date :: Monad m => Builtin m
builtin_time_date = Builtin
    (Ty.number 🡒 Ty.out (Ty.arrayOf Ty.number)) $ pure $
    \(Cons ns Nil) ->
    let utc       = nsToUtc ns
        (y, m, d) = Time.toGregorian (Time.utctDay utc) in
    return ([fromIntegral y, m, d] :: [Int])

builtin_time_parse_rfc3339_ns :: Monad m => Builtin m
builtin_time_parse_rfc3339_ns = Builtin
    (Ty.string 🡒 Ty.out Ty.number) $ pure $
    \(Cons txt Nil) -> case Time.RFC3339.parseTimeRFC3339 txt of
        Just zoned -> return $! utcToNs $ Time.zonedTimeToUTC zoned
        Nothing    -> throwString $
            "Could not parse RFC3339 time: " ++ T.unpack txt
