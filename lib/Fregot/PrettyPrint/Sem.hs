{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

Semantic pretty-printer
-}
{-# LANGUAGE DeriveGeneric #-}
module Fregot.PrettyPrint.Sem
    ( Sem (..)
    , SemDoc

    , error
    , header
    , hint
    , code
    , keyword
    , literal
    , namespace
    , punctuation

    , hPutSemDoc
    , renderSpanDoc
    ) where

import           Data.Char                   (toLower)
import qualified Data.Text                   as T
import           Fregot.PrettyPrint.Internal
import           GHC.Generics                (Generic)
import           Prelude                     hiding (error)
import qualified System.Console.ANSI         as Ansi
import qualified System.IO.Extended          as IO

data Sem
    = Error
    | Header
    | Hint
    | Code

    | Keyword
    | Literal
    | Namespace
    | Punctuation
    deriving (Generic, Show)

type SemDoc = Doc Sem

semToHtmlClass :: Sem -> T.Text
semToHtmlClass sem = T.pack $ "fregot-" ++ map toLower (show sem)

error :: SemDoc -> SemDoc
error = annotate Error

header :: SemDoc -> SemDoc
header = annotate Header

hint :: SemDoc -> SemDoc
hint = annotate Hint

code :: SemDoc -> SemDoc
code = annotate Code

keyword, literal, namespace, punctuation :: SemDoc -> SemDoc
keyword     = annotate Keyword
literal     = annotate Literal
namespace   = annotate Namespace
punctuation = annotate Punctuation

semToColor :: Sem -> Ansi.Color
semToColor Error       = Ansi.Red
semToColor Header      = Ansi.Blue
semToColor Hint        = Ansi.Green
semToColor Code        = Ansi.Yellow

semToColor Keyword     = Ansi.Blue
semToColor Literal     = Ansi.Red
semToColor Namespace   = Ansi.Magenta
semToColor Punctuation = Ansi.Cyan

hPutSemDoc :: IO.Handle -> Doc Sem -> IO ()
hPutSemDoc h = hPutColDoc h . fmap semToColor

renderSpanDoc :: Doc Sem -> T.Text
renderSpanDoc = renderSpanSimpleDoc . renderDefaults . fmap semToHtmlClass
