module Text.Pcre2
    ( Internal.PcreException

    , Internal.CompileOptions (..)
    , Internal.defaultCompileOptions

    , Internal.Regex
    , compile
    , compileWith

    , Internal.Match (..)
    , Internal.Range (..)
    , match
    , matchWith
    ) where

import           Control.Exception   (try)
import qualified Data.Text           as T
import           System.IO.Unsafe    (unsafePerformIO)
import qualified Text.Pcre2.Internal as Internal

compile
    :: T.Text -> Either Internal.PcreException Internal.Regex
compile = compileWith Internal.defaultCompileOptions

compileWith
    :: Internal.CompileOptions -> T.Text
    -> Either Internal.PcreException Internal.Regex
compileWith opts = unsafePerformIO . try . Internal.compile opts

match
    :: Internal.Regex -> T.Text
    -> Either Internal.PcreException [Internal.Match]
match = matchWith Internal.defaultMatchOptions

matchWith
    :: Internal.MatchOptions -> Internal.Regex -> T.Text
    -> Either Internal.PcreException [Internal.Match]
matchWith opts regex = unsafePerformIO . try . Internal.match opts regex
