{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Fregot.Error.Stack
    ( StackTrace
    , StackFrame (..)
    , empty
    , null
    , push
    , peek
    , isStepOver
    ) where

import qualified Data.List                 as L
import           Data.Maybe                (listToMaybe)
import           Fregot.PrettyPrint        ((<+>))
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Fregot.Sugar              (PackageName, Var)
import           Prelude                   hiding (null)

newtype StackTrace = StackTrace [StackFrame]
    deriving (Eq, Show)

instance PP.Pretty PP.Sem StackTrace where
    pretty (StackTrace frames) = PP.vcat $ map PP.pretty frames

data StackFrame
    = RuleStackFrame PackageName Var SourceSpan
    | FunctionStackFrame PackageName Var SourceSpan
    deriving (Eq, Show)

instance PP.Pretty PP.Sem StackFrame where
    pretty = \case
        RuleStackFrame _ rule source ->
            "rule" <+> PP.code (PP.pretty rule) <+> "at" <+> PP.pretty source
        FunctionStackFrame _ fun source ->
            "function" <+> PP.code (PP.pretty fun) <+> "at" <+> PP.pretty source

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
