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
      SourceSpan (..), sourcePointer, start, end
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

-- | "foo.lw" (line 42, column 10)
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
        caretty (ss ^. start . column) (ss ^. end . column)
    _ -> Just $ PP.vcat
        [ prefix i <> PP.pretty x
        | (i,x) <- zip [ss ^. start . line .. ] sInput
        ]
  where
    prefix i        = justifyRight prefixWidth (T.pack (show i) <> "| ")
    prefixWidth     = length (show $ ss ^. end . line) + 2
    caretty c1 c2   = PP.indent (prefixWidth + c1 - 1) $
                          decorate (PP.pretty $ T.replicate (c2 - c1 + 1) "^")

    sInput = sourceSpanInput sources ss

    justifyRight :: Int -> T.Text -> PP.SemDoc
    justifyRight i txt
        | T.length txt >= i = PP.pretty txt
        | otherwise         = PP.indent (i - T.length txt) (PP.pretty txt)

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
