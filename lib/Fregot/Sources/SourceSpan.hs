{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Sources.SourceSpan
    ( -- * Snippets
      SourcePointer (..)
    , SourceSpan (..), sourcePointer, start, end
    , Position (..), line, column

    , prettyPos
    , prettyLoc
    , prettySourceSpan
    , citeSourceSpan

    , sourceSpanToSourcePos
    , sourcePosToSourceSpan

    , unsafeMergeSourceSpan
    , testSourceSpan
    ) where

import           Control.Lens          ((&), (.~), (^.))
import           Control.Lens.TH       (makeLenses)
import qualified Data.Aeson            as Aeson
import           Data.Binary           (Binary)
import           Data.Char             (isSpace)
import           Data.Maybe            (fromMaybe, isNothing)
import qualified Data.Text             as T
import           Data.Typeable         (Typeable)
import           Fregot.Lexer.Position
import           Fregot.PrettyPrint    ((<$$$>?), (<$$>), (<+>))
import qualified Fregot.PrettyPrint    as PP
import           Fregot.Sources        as Sources
import           GHC.Generics          (Generic)
import qualified Text.Parsec           as Parsec
import qualified Text.Parsec.Pos       as Parsec

data SourceSpan = SourceSpan
    { _sourcePointer ::                !SourcePointer
    , _start         :: {-# UNPACK #-} !Position
    , _end           :: {-# UNPACK #-} !Position
    } deriving (Eq, Generic, Ord, Show, Typeable)

instance Binary SourceSpan

$(makeLenses ''SourceSpan)

instance PP.Pretty a SourceSpan where
    pretty ss =
        PP.pretty (describeSourcePointer $ ss ^. sourcePointer) <> ":" <>
        PP.pretty (ss ^. start . line) <> ":" <>
        PP.pretty (ss ^. start . column)

instance Aeson.ToJSON SourceSpan where
    toJSON e = Aeson.object
        [ "sourcePointer" Aeson..= (e ^. sourcePointer)
        , "start"         Aeson..= (e ^. start)
        , "end"           Aeson..= (e ^. end)
        ]

-- | (line 42, column 10)
prettyPos :: SourceSpan -> PP.Doc a
prettyPos ss = PP.parens $
    "line"   <+> PP.pretty (ss ^. start . line) <> "," <+>
    "column" <+> PP.pretty (ss ^. start . column)

-- | "foo.rego" (line 42, column 10)
prettyLoc :: SourceSpan -> PP.Doc a
prettyLoc ss =
    PP.dquotes (PP.pretty $ describeSourcePointer $ ss ^. sourcePointer) <+>
    prettyPos ss

prettySourceSpan
    :: Sources.Sources -> SourceSpan -> PP.SemDoc -> Maybe PP.SemDoc
    -> PP.SemDoc
prettySourceSpan sources ss title doc =
    (prettyLoc ss <> ":") <$$>
    title' <$$$>? snip <$$$>? doc
  where
    title' = if isNothing doc && isNothing snip then title else title <> ":"
    snip   = fmap PP.ind $ citeSourceSpan PP.error sources ss

citeSourceSpan
    :: (PP.SemDoc -> PP.SemDoc) -> Sources.Sources -> SourceSpan
    -> Maybe PP.SemDoc
citeSourceSpan decorate sources ss = case sInput of
    [] -> Nothing
    [x] -> Just $
        prefix (ss ^. start . line) <> PP.pretty x <$$>
        caretty x (ss ^. start . column) (ss ^. end . column)
    _ -> Just $ PP.vcat
        [ prefix i <> PP.pretty x
        | (i,x) <- zip [ss ^. start . line .. ] sInput
        ]
  where
    -- A line number prefix, e.g. "42| ".  Since we may have prefixes of
    -- different length, we need to calculate the width of the longest one and
    -- align them.
    prefix i = justifyRight prefixw (T.pack (show i) <> "| ")
    prefixw  = length (show $ ss ^. end . line) + 2

    -- Produce a "^^^^^" line running from column c1 to column c2 (inclusive).
    caretty x c1 c2 =
        PP.pretty (T.replicate prefixw " ") <>
        PP.pretty (takeSpacesFromTemplate (c1 - 1) x) <>
        decorate (PP.pretty $ T.replicate (c2 - c1 + 1) "^")

    -- The actual input text.
    sInput = sourceSpanInput sources ss

    justifyRight :: Int -> T.Text -> PP.SemDoc
    justifyRight i txt
        | T.length txt >= i = PP.pretty txt
        | otherwise         = PP.indent (i - T.length txt) (PP.pretty txt)

    -- Parsec counts tabs in a somewhat complicated way, so our strategy is to
    -- take the space parts of the original line and reuse that literally.  We
    -- create a virtual parsec position and use that to get exactly the same
    -- count.  If the line is not long enough to take spaces from it, we'll just
    -- use normal spaces.
    takeSpacesFromTemplate :: Int -> T.Text -> T.Text
    takeSpacesFromTemplate n =
        let go p template
                | Parsec.sourceColumn p - 1 >= n = ""
                | Just (h, t) <- T.uncons template, isSpace h =
                    T.singleton h <> go (Parsec.updatePosChar p h) t
                | otherwise = T.replicate (n - Parsec.sourceColumn p + 1) " " in
        go (Parsec.newPos "virtual" 1 1)

sourceSpanInput :: Sources.Sources -> SourceSpan -> [T.Text]
sourceSpanInput sourceStore ss =
    take ((ss ^. end . line) - (ss ^. start . line) + 1) $
    drop ((ss ^. start . line) - 1) $
    T.lines source
  where
    source :: T.Text
    source = fromMaybe "" $ Sources.lookup (ss ^. sourcePointer) sourceStore

sourceSpanToSourcePos :: SourceSpan -> Parsec.SourcePos
sourceSpanToSourcePos ss = Parsec.newPos
    (describeSourcePointer (ss ^. sourcePointer))
    (ss ^. start . line)
    (ss ^. start . column)

sourcePosToSourceSpan :: SourcePointer -> Parsec.SourcePos -> SourceSpan
sourcePosToSourceSpan sp pos = SourceSpan
    { _sourcePointer = sp
    , _start         = p
    , _end           = p
    }
  where
    p = Position (Parsec.sourceLine pos) (Parsec.sourceColumn pos)

unsafeMergeSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
unsafeMergeSourceSpan x y
    | x ^. sourcePointer /= y ^. sourcePointer = error
        "Fregot.Sources.unsafeMergeSourceSpan: source pointer mismatch"
    | otherwise = x
        & start .~ (min (x ^. start) (y ^. start))
        & end   .~ (max (x ^. end)   (y ^. end))

-- | Source span that can be used for calling tests.
testSourceSpan :: SourceSpan
testSourceSpan = SourceSpan TestInput initPosition initPosition
