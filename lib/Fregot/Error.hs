{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
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
module Fregot.Error
    ( SourceSpanMessage (..), sourceSpan, title, body

    , Severity (..)
    , Subsystem
    , Error (..), severity, subsystem, sourceSpans, details, hints, stack

    , Errors
    , severe

    , Format (..)
    , hPutErrors
    , errorToJson

    , fromParsecError
    , fromParsecError'

    , mkError
    , mkErrorNoMeta
    , mkMultiError

    , catchIO
    ) where

import           Control.Exception          (try)
import           Control.Lens               (anyOf, preview, (^.), _head)
import           Control.Lens.TH            (makeLenses)
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.Trans        (MonadIO (..))
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Data                  (Data)
import           Data.List                  (sortBy)
import           Data.List.NonEmpty         (NonEmpty)
import           Data.Ord                   (comparing)
import qualified Data.Text                  as T
import qualified Fregot.Error.Stack         as Stack
import           Fregot.Main.GlobalOptions  (Format (..))
import           Fregot.PrettyPrint         ((<$$>), (<+>), (?<+>))
import qualified Fregot.PrettyPrint         as PP
import qualified Fregot.Sources             as Sources
import           Fregot.Sources.SourceSpan
import           GHC.Generics               (Generic)
import qualified System.IO                  as IO
import qualified System.IO.Error            as IO
import qualified Text.Parsec                as Parsec

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

instance Aeson.ToJSON Severity where
    toJSON FatalSeverity   = "error"
    toJSON ErrorSeverity   = "error"
    toJSON WarningSeverity = "warning"

type Subsystem = T.Text

data Error = Error
  { _severity    :: !Severity
  , _subsystem   :: !Subsystem
  , _sourceSpans :: ![SourceSpanMessage]
  , _details     :: ![PP.SemDoc]
  , _hints       :: ![PP.SemDoc]
  , _stack       :: !Stack.StackTrace
  } deriving (Show, Generic)

$(makeLenses ''SourceSpanMessage)
$(makeLenses ''Error)

instance Aeson.ToJSON SourceSpanMessage where
    toJSON e = Aeson.object
        [ "sourceSpan" Aeson..= (e ^. sourceSpan)
        , "title"      Aeson..= (e ^. title)
        , "body"       Aeson..= (e ^. body)
        ]

errorToJson :: Sources.Sources -> Error -> Aeson.Value
errorToJson sources err = Aeson.object
    [ "severity"    Aeson..= (err ^. severity)
    , "subsystem"   Aeson..= (err ^. subsystem)
    , "sourceSpans" Aeson..= (err ^. sourceSpans)
    , "details"     Aeson..= (err ^. details)
    , "hints"       Aeson..= (err ^. hints)
    , "stack"       Aeson..= (err ^. stack)
    , "_text"       Aeson..= PP.toText (prettyError sources err)
    ]

-- | It's fine, it's only a warning...
severe :: Traversable f => f Error -> Bool
severe = anyOf (traverse . severity) (/= WarningSeverity)

-- | In a lot of cases, we allow functions to throw a list of errors.  We
-- shouldn't allow these functions to throw the empty list though.
type Errors = NonEmpty Error

hPutErrors
    :: IO.Handle -> Sources.Sources -> Format -> [Error] -> IO ()
hPutErrors handle sources fmt messages0 = case fmt of
    -- NOTE (jaspervdj): If we do not have any messages, we don not want to
    -- print anything in case we have text output (`hPutSemDoc` introduces a
    -- newline character).  This is different from the JSON format, where we
    -- need to print `[]` regardless of whether or not we have any messages.
    Text
        | null messages0 -> return ()
        | otherwise      -> PP.hPutSemDoc handle $
            prettyErrors sources messages1
    Json -> do
        BL.hPutStr handle $ Aeson.encode $ map (errorToJson sources) messages1
        BL8.hPutStrLn handle ""
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
        [PP.hint "Hint:" <$$> PP.ind x | x <- err ^. hints] ++
        (if Stack.null (err ^. stack)
            then []
            else ["Stack trace:" <$$> PP.ind (PP.pretty (err ^. stack))]))

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
    , _stack       = Stack.empty
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

mkErrorNoMeta :: Subsystem -> PP.SemDoc -> Error
mkErrorNoMeta sub body' = Error
    { _severity    = ErrorSeverity
    , _subsystem   = sub
    , _details     = [body']
    , _hints       = []
    , _sourceSpans = []
    , _stack       = Stack.empty
    }

mkMultiError :: Subsystem -> PP.SemDoc -> [(SourceSpan, PP.SemDoc)] -> Error
mkMultiError sub title' bodies = Error
    { _severity    = ErrorSeverity
    , _subsystem   = sub
    , _details     = []
    , _hints       = []
    , _stack       = Stack.empty
    , _sourceSpans = do
        (ss, body') <- bodies
        return SourceSpanMessage
            { _sourceSpan = ss
            , _title      = title'
            , _body       = Just body'
            }
    }

-- | Perform the IO action, raising IO exceptions as an `Error` instead.
--
-- TODO(jaspervdj): Add an optional SourceSpan parameter?
catchIO :: (MonadError [Error] m, MonadIO m) => IO a -> m a
catchIO mio = do
    errOrX <- liftIO $ try mio
    case errOrX of
        Left  e -> throwError [fromIOException e]
        Right x -> return x

fromIOException :: IO.IOError -> Error
fromIOException e = Error
    { _severity     = FatalSeverity
    , _subsystem   = "io"
    , _sourceSpans = []
    , _hints       = []
    , _details     = [doc]
    , _stack       = Stack.empty
    }
  where
    doc | errorType == IO.doesNotExistErrorType =
            "No such file:" <+> maybe "<unknown>" PP.pretty fileName
        | otherwise =
             PP.keyword (PP.pretty $ show errorType) <> ":" <$$>
             PP.pretty errorString <>
             maybe "" (\fn -> " (" <> PP.pretty fn <> ")") fileName

    errorType   = IO.ioeGetErrorType e
    errorString = IO.ioeGetErrorString e
    fileName    = IO.ioeGetFileName e
