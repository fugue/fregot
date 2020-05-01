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

    , Sig (..)

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

import qualified Fregot.Builtins.Base64   as Builtins.Base64
import qualified Fregot.Builtins.Basics   as Builtins.Basics
import           Fregot.Builtins.Internal
import qualified Fregot.Builtins.Json     as Builtins.Json
import qualified Fregot.Builtins.Regex    as Builtins.Regex
import qualified Fregot.Builtins.Time     as Builtins.Time

defaultBuiltins :: Builtins IO
defaultBuiltins =
    Builtins.Base64.builtins <>
    Builtins.Basics.builtins <>
    Builtins.Json.builtins <>
    Builtins.Regex.builtins <>
    Builtins.Time.builtins
