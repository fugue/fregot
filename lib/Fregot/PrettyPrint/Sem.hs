-- | Semantic pretty-printer
{-# LANGUAGE DeriveGeneric #-}
module Fregot.PrettyPrint.Sem
    ( Sem (..)
    , SemDoc

    , error
    , header
    , hint
    , code
    , literal
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

    | Literal
    | Punctuation
    deriving (Generic, Show)

type SemDoc = Doc Sem

semToHtmlClass :: Sem -> T.Text
semToHtmlClass sem = T.pack $ "lwc-" ++ map toLower (show sem)

error :: SemDoc -> SemDoc
error = annotate Error

header :: SemDoc -> SemDoc
header = annotate Header

hint :: SemDoc -> SemDoc
hint = annotate Hint

code :: SemDoc -> SemDoc
code = annotate Code

literal, punctuation :: SemDoc -> SemDoc
literal     = annotate Literal
punctuation = annotate Punctuation

semToColor :: Sem -> Ansi.Color
semToColor Error       = Ansi.Red
semToColor Header      = Ansi.Blue
semToColor Hint        = Ansi.Green
semToColor Code        = Ansi.Yellow
semToColor Literal     = Ansi.Red
semToColor Punctuation = Ansi.Yellow

hPutSemDoc :: IO.Handle -> Doc Sem -> IO ()
hPutSemDoc h = hPutColDoc h . fmap semToColor

renderSpanDoc :: Doc Sem -> T.Text
renderSpanDoc = renderSpanSimpleDoc . renderDefaults . fmap semToHtmlClass
