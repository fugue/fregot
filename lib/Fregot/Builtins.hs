{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
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
module Fregot.Builtins
    ( ToVal (..)
    , FromVal (..)

    , Args (..)
    , toArgs

    , BuiltinM
    , BuiltinException (..)
    , Builtin (..)
    , ReadyBuiltin
    , arity

    , Function (..)
    , Builtins
    , defaultBuiltins
    ) where

import qualified Fregot.Builtins.Base64          as Builtins.Base64
import qualified Fregot.Builtins.Basics          as Builtins.Basics
import qualified Fregot.Builtins.Graph           as Builtins.Graph
import           Fregot.Builtins.Internal
import qualified Fregot.Builtins.Json            as Builtins.Json
import qualified Fregot.Builtins.Jwt             as Builtins.Jwt
import qualified Fregot.Builtins.Object          as Builtins.Object
import qualified Fregot.Builtins.Regex           as Builtins.Regex
import qualified Fregot.Builtins.RegexGlobsMatch as Builtins.RegexGlobsMatch
import qualified Fregot.Builtins.Time            as Builtins.Time
import qualified Fregot.Builtins.Yaml            as Builtins.Yaml

defaultBuiltins :: Builtins IO
defaultBuiltins =
    Builtins.Base64.builtins <>
    Builtins.Basics.builtins <>
    Builtins.Graph.builtins <>
    Builtins.Json.builtins <>
    Builtins.Jwt.builtins <>
    Builtins.Object.builtins <>
    Builtins.Regex.builtins <>
    Builtins.RegexGlobsMatch.builtins <>
    Builtins.Time.builtins <>
    Builtins.Yaml.builtins
