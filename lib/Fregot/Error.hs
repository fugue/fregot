{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Error
    ( SourceSpanMessage (..), sourceSpan, title, body

    , Severity (..)
    , Subsystem
    , Error (..), severity, subsystem, sourceSpans, details, hints

    , Errors
    , severe

    , ErrorFmt (..)
    , hPutErrors

    , fromParsecError
    , fromParsecError'

    , mkError
    , mkMultiError
    ) where

import           Control.Lens              (preview, (^.), _head, anyOf)
import           Control.Lens.TH           (makeLenses)
import           Data.Data                 (Data)
import           Data.List                 (sortBy)
import           Data.List.NonEmpty        (NonEmpty)
import           Data.Monoid               ((<>))
import           Data.Ord                  (comparing)
import qualified Data.Text                 as T
import           Fregot.PrettyPrint        ((<$$>), (<+>), (?<+>))
import qualified Fregot.PrettyPrint        as PP
import qualified Fregot.Sources            as Sources
import           Fregot.Sources.SourceSpan
import           GHC.Generics              (Generic)
import qualified System.IO                 as IO
import qualified Text.Parsec               as Parsec

-- | Part of a message that belongs to a specific location.
data SourceSpanMessage = SourceSpanMessage
  { _sourceSpan :: !SourceSpan
  , _title      :: !PP.SemDoc
  , _body       :: !(Maybe PP.SemDoc)
  } deriving (Show, Generic)

data Severity
    = FatalSeverity
    | ErrorSeverity
    | WarningSeverity
    deriving (Eq, Data, Show, Generic)

instance PP.Pretty e Severity where
    pretty FatalSeverity   = "fatal error"
    pretty ErrorSeverity   = "error"
    pretty WarningSeverity = "warning"

type Subsystem = T.Text

data Error = Error
  { _severity    :: !Severity
  , _subsystem   :: !Subsystem
  , _sourceSpans :: ![SourceSpanMessage]
  , _details     :: ![PP.SemDoc]
  , _hints       :: ![PP.SemDoc]
  } deriving (Show, Generic)

$(makeLenses ''SourceSpanMessage)
$(makeLenses ''Error)

-- | It's fine, it's only a warning...
severe :: Traversable f => f Error -> Bool
severe = anyOf (traverse . severity) (/= WarningSeverity)

-- | In a lot of cases, we allow functions to throw a list of errors.  We
-- shouldn't allow these functions to throw the empty list though.
type Errors = NonEmpty Error

data ErrorFmt = TextFmt deriving (Show, Eq)

hPutErrors
    :: IO.Handle -> Sources.Sources -> ErrorFmt -> [Error] -> IO ()
hPutErrors handle sources fmt messages0 = case fmt of
    -- NOTE (jaspervdj): If we do not have any messages, we don not want to
    -- print anything in case we have text output (`hPutSemDoc` introduces a
    -- newline character).  This is different from the JSON format, where we
    -- need to print `[]` regardless of whether or not we have any messages.
    TextFmt
        | null messages0 -> return ()
        | otherwise      -> PP.hPutSemDoc handle $
            prettyErrors sources messages1
  where
    sortKey   = preview (sourceSpans . _head . sourceSpan)
    messages1 = sortBy (comparing sortKey) messages0

prettyErrors :: Sources.Sources -> [Error] -> PP.SemDoc
prettyErrors sources = PP.vcat2 . map (prettyError sources)

prettyError :: Sources.Sources -> Error -> PP.SemDoc
prettyError sources err =
    "fregot" <+>
    PP.parens (PP.header $
        (case err ^. severity of
            FatalSeverity -> Just "fatal"
            _             -> Nothing) ?<+>
        PP.pretty (err ^. subsystem) <+>
        (case err ^. severity of
            WarningSeverity -> "warning"
            _               -> "error")) <>
    ":" <$$>
    PP.ind (PP.vcat2 $
        [prettySourceSpanMessage sources x | x <- err ^. sourceSpans] ++
        (err ^. details) ++
        [PP.hint "Hint:" <$$> PP.ind x | x <- err ^. hints])

prettySourceSpanMessage
    :: Sources.Sources -> SourceSpanMessage -> PP.SemDoc
prettySourceSpanMessage sources x =
    prettySourceSpan sources (x ^. sourceSpan) (x ^. title) (x ^. body)

fromParsecError
    :: Severity -> Sources.SourcePointer -> SourceSpan -> Parsec.ParseError
    -> Error
fromParsecError sev sp ss e = Error
    { _severity    = sev
    , _subsystem   = "parse"
    , _details     = []
    , _hints       = []
    , _sourceSpans = return $ SourceSpanMessage
        { _sourceSpan = ss
        , _title      = "parse failed"
        , _body       = Just $ PP.vcat
            [ PP.pretty l
            | l <- tail $ lines $ show e
            ] <>
            -- Include exact location again UNLESS that is equal to meta1,
            -- because then it would be silly to repeat it.
            (if ssexact == ss
                then PP.empty
                else PP.line <> "at " <> prettyPos ssexact)
        }
    }
  where
    -- Ensure pandoc snippet is included in the metadata.
    ssexact = sourcePosToSourceSpan sp $ Parsec.errorPos e

fromParsecError'
    :: Severity -> Sources.SourcePointer -> Parsec.ParseError -> Error
fromParsecError' sev sp e =
    fromParsecError sev sp (sourcePosToSourceSpan sp $ Parsec.errorPos e) e

mkError :: Subsystem -> SourceSpan -> PP.SemDoc -> PP.SemDoc -> Error
mkError sub ss title' body' = mkMultiError sub title' [(ss, body')]

mkMultiError :: Subsystem -> PP.SemDoc -> [(SourceSpan, PP.SemDoc)] -> Error
mkMultiError sub title' bodies = Error
    { _severity    = ErrorSeverity
    , _subsystem   = sub
    , _details     = []
    , _hints       = []
    , _sourceSpans = do
        (ss, body') <- bodies
        return SourceSpanMessage
            { _sourceSpan = ss
            , _title      = title'
            , _body       = Just body'
            }
    }
