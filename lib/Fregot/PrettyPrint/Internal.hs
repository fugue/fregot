{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Fregot.PrettyPrint.Internal
    ( module Text.PrettyPrint.Annotated.Leijen

      -- * Extra combinators
    , ind
    , indDocs
    , commaSep
    , parensSepVert
    , nth
    , num
    , plural
    , vcat2

      -- * Extra operators
    , (<>)
    , (<$$>?)
    , (?<$$>)

    , (<+>?)
    , (?<+>)

    , (<$$$>)
    , (<$$$>?)
    , (?<$$$>)

      -- * Typeclasses
    , Pretty (..)

      -- * Outputting
    , toText
    , toLazyText
    , hPutColDoc
    , renderSpanSimpleDoc
    , renderDefaults
    ) where

import qualified Data.Aeson                        as Aeson
import           Data.Semigroup                    (Semigroup, (<>))
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as TL
import           Prelude                           hiding (error)
import qualified System.Console.ANSI               as Ansi
import qualified System.IO.Extended                as IO
import qualified Data.Scientific as Scientific
import qualified Text.Blaze.Html                   as Html
import qualified Text.Blaze.Html.Renderer.Text     as Html
import           Text.PrettyPrint.Annotated.Leijen hiding ((<>))
import qualified Text.PrettyPrint.Annotated.Leijen as Leijen

instance Semigroup (Doc a) where
    (<>) = (Leijen.<>)

instance Monoid (Doc a) where
    mempty  = empty
    mappend = (Leijen.<>)

instance Aeson.ToJSON (Doc a) where
    toJSON = Aeson.toJSON . toText

-- | Like 'indent', but with a default indentation of 2 spaces.
ind :: Doc a -> Doc a
ind = indent 2

-- | 'indent' a bunch of other docs with a default indentation of 2 spaces.
indDocs :: [Doc a] -> Doc a
indDocs = ind . vcat

-- | Horizontally comma-separated.
commaSep :: [Doc a] -> Doc a
commaSep []           = empty
commaSep [x]          = x
commaSep (x : y : zs) = x <> "," <+> commaSep (y : zs)

-- | Vertical braces, comma-separated.
parensSepVert :: Doc a -> Doc a -> Doc a -> [Doc a] -> Doc a
parensSepVert open close s xs = case xs of
  []  -> open <> close
  [x] -> open <> x <> close
  _   -> open <$$> ind (sepVert s xs) <$$> close

-- | Vertically comma-separated.
sepVert :: Doc a -> [Doc a] -> Doc a
sepVert _ []           = empty
sepVert _ [x]          = x
sepVert s (x : y : zs) = x <> s <$$> sepVert s (y : zs)

nth :: Int -> Doc a
nth 1 = "first"
nth 2 = "second"
nth 3 = "third"
nth n = pretty n <> "th"

num :: Int -> Doc a
num 0 = "zero"
num 1 = "one"
num 2 = "two"
num 3 = "three"
num n = pretty n

plural :: Int -> Doc a -> Doc a -> Doc a
plural n single more = case n of
    1 -> num n <+> single
    _ -> num n <+> more

vcat2 :: [Doc a] -> Doc a
vcat2 []           = empty
vcat2 [x]          = x
vcat2 (x : y : ys) = x <$$$> vcat2 (y : ys)

(<+>?) :: Doc a -> Maybe (Doc a) -> Doc a
x <+>? Nothing = x
x <+>? Just y  = x <+> y
infixl 7 <+>?  -- <+> is 6

(?<+>) :: Maybe (Doc a) -> Doc a -> Doc a
Nothing ?<+> y = y
Just x  ?<+> y = x <+> y
infixl 7 ?<+>  -- <+> is 6

(<$$>?) :: Doc a -> Maybe (Doc a) -> Doc a
x <$$>? Nothing = x
x <$$>? Just y  = x <$$> y
infixl 7 <$$>?

(?<$$>) :: Maybe (Doc a) -> Doc a -> Doc a
Nothing ?<$$> y = y
Just x  ?<$$> y = x <$$> y
infixl 7 ?<$$>

-- Like '<$$>' but separate with two newlines.
(<$$$>) :: Doc a -> Doc a -> Doc a
x <$$$> y = x <> line <> line <> y
infixr 5 <$$$>

(<$$$>?) :: Doc a -> Maybe (Doc a) -> Doc a
x <$$$>? Nothing = x
x <$$$>? Just y  = x <$$$> y
infixl 7 <$$$>?

(?<$$$>) :: Maybe (Doc a) -> Doc a -> Doc a
Nothing ?<$$$> y = y
Just x  ?<$$$> y = x <$$$> y
infixl 7 ?<$$$>

class Pretty a p where
    pretty :: p -> Doc a

instance Pretty a String where
    pretty = string

instance Pretty a Int where
    pretty = int

instance Pretty a Integer where
    pretty = integer

instance Pretty a Double where
    pretty = double

instance Pretty a Scientific.Scientific where
    pretty s = case Scientific.floatingOrInteger s of
        Left  x -> pretty (x :: Double)
        Right x -> pretty (x :: Integer)

instance Pretty a T.Text where
    pretty = string . T.unpack

instance Pretty a TL.Text where
    pretty = string . TL.unpack

toText :: Doc a -> T.Text
toText = T.pack . display . renderDefaults

-- | Pretty-printer with reasonable defaults.
toLazyText :: Doc a -> TL.Text
toLazyText = TL.fromStrict . toText

hPutColDoc :: IO.Handle -> Doc Ansi.Color -> IO ()
hPutColDoc h = hPutSimpleColDoc h . renderDefaults

hPutSimpleColDoc :: IO.Handle -> SimpleDoc Ansi.Color -> IO ()
hPutSimpleColDoc h = IO.hWithEncoding h IO.utf8 . go
  where
    go SEmpty = IO.hPutStrLn h "" >> IO.hFlush h
    go (SChar c doc) =
        IO.hPutChar h c >> go doc
    go (SText _width str doc) =
        IO.hPutStr h str >> go doc
    go (SLine i doc) =
        IO.hPutStrLn h "" >> IO.hPutStr h (replicate i ' ') >> go doc
    go (SAnnotStart col doc) =
        Ansi.hSetSGR h [Ansi.SetColor Ansi.Foreground Ansi.Dull col] >> go doc
    go (SAnnotStop doc) =
        Ansi.hSetSGR h [Ansi.Reset] >> go doc

renderDefaults :: Doc a -> SimpleDoc a
renderDefaults = removeIndentOnly . renderPretty 0.9 80

-- | Remove lines with only indentation.
removeIndentOnly :: SimpleDoc a -> SimpleDoc a
removeIndentOnly = go
  where
    go SEmpty                    = SEmpty
    go (SChar c doc)             = SChar c (go doc)
    go (SText w str doc)         = SText w str (go doc)
    go (SLine _ doc@(SLine _ _)) = SLine 0 (go doc)
    go (SLine i doc)             = SLine i (go doc)
    go (SAnnotStart ann doc)     = SAnnotStart ann (go doc)
    go (SAnnotStop doc)          = SAnnotStop (go doc)

renderSpanSimpleDoc :: SimpleDoc T.Text -> T.Text
renderSpanSimpleDoc = TL.toStrict . Html.renderHtml . go
  where
    go :: SimpleDoc T.Text -> Html.Html
    go SEmpty = "\n"
    go (SLine n doc) = "\n" <> mconcat (replicate n " ") <> go doc
    go (SChar c doc) = Html.toHtml c <> go doc
    go (SText _ t doc) = Html.toHtml t <> go doc
    go (SAnnotStart klass doc) =
        Html.preEscapedText ("<span class=\"" <> klass <> "\">") <> go doc
    go (SAnnotStop doc) =
        Html.preEscapedText "</span>" <> go doc
