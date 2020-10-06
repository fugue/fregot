{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Data.ByteString.Extended
    ( module Data.ByteString
    , replace
    ) where

import           Data.ByteString
import           Data.Monoid     ((<>))

replace :: ByteString -> ByteString -> ByteString -> ByteString
replace pattern replacement haystack =
    go haystack
  where
    patlen = length pattern
    go bs  =
        let !(!xs, !ys) = breakSubstring pattern bs in
        if null ys then xs else xs <> replacement <> go (drop patlen ys)
