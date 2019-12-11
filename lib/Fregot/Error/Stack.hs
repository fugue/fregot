{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Error.Stack
    ( StackTrace
    , StackFrame (..)
    , empty
    , null
    , push
    , peek
    , isStepOver
    , package
    ) where

import           Control.Lens              ((^?), _1)
import qualified Data.Aeson                as Aeson
import qualified Data.List                 as L
import           Data.Maybe                (listToMaybe)
import           Fregot.Names
import           Fregot.PrettyPrint        ((<+>))
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Prelude                   hiding (null)

newtype StackTrace = StackTrace [StackFrame]
    deriving (Eq, Show, Aeson.ToJSON)

instance PP.Pretty PP.Sem StackTrace where
    pretty (StackTrace frames) = PP.vcat $ map PP.pretty frames

data StackFrame
    = RuleStackFrame     Name SourceSpan
    | FunctionStackFrame Name SourceSpan
    deriving (Eq, Show)

instance PP.Pretty PP.Sem StackFrame where
    pretty = \case
        RuleStackFrame rule source ->
            "rule" <+> PP.code (PP.pretty rule) <+> "at" <+> PP.pretty source
        FunctionStackFrame fun source ->
            "function" <+> PP.code (PP.pretty fun) <+> "at" <+> PP.pretty source

instance Aeson.ToJSON StackFrame where
    toJSON = \case
        RuleStackFrame rule source -> Aeson.object
            [ "type"   Aeson..= ("rule" :: String)
            , "name"   Aeson..= rule
            , "source" Aeson..= source
            ]
        FunctionStackFrame fun source -> Aeson.object
            [ "type"   Aeson..= ("function" :: String)
            , "name"   Aeson..= fun
            , "source" Aeson..= source
            ]

-- | NOTE(jaspervdj): The 'Maybe' is mainly a future-compatible thing.
name :: StackFrame -> Maybe Name
name (RuleStackFrame     n _) = Just n
name (FunctionStackFrame n _) = Just n

empty :: StackTrace
empty = StackTrace mempty

null :: StackTrace -> Bool
null (StackTrace s) = L.null s

push :: StackFrame -> StackTrace -> StackTrace
push frame (StackTrace frames) = StackTrace (frame : frames)

peek :: StackTrace -> Maybe StackFrame
peek (StackTrace frames) = listToMaybe frames

-- | Some logic to decide if a new stack trace "steps over" an old one in a
-- debugger.
isStepOver :: StackTrace -> StackTrace -> Bool
isStepOver (StackTrace newFrames) (StackTrace oldFrames) =
    case L.stripPrefix (reverse oldFrames) (reverse newFrames) of
        Just (_ : _) -> False
        Just []      -> True
        Nothing      -> True

-- | Determine what package we are in depending on the stack trace.
package :: StackTrace -> Maybe PackageName
package stack = peek stack >>= name >>=
    (^? _QualifiedName . qualifiedVarFromKey . _1)
