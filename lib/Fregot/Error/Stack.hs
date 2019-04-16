{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Fregot.Error.Stack
    ( StackTrace
    , StackFrame (..)
    , empty
    , null
    ) where

import qualified Data.List                 as L
import           Fregot.PrettyPrint        ((<+>))
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Fregot.Sugar              (Var)
import           Prelude                   hiding (null)

newtype StackTrace = StackTrace [StackFrame]
    deriving (Show)

instance PP.Pretty PP.Sem StackTrace where
    pretty (StackTrace frames) = PP.vcat $ map PP.pretty frames

data StackFrame
    = RuleStackFrame Var SourceSpan
    deriving (Show)

instance PP.Pretty PP.Sem StackFrame where
    pretty = \case
        RuleStackFrame rule source ->
            "rule " <+> PP.code (PP.pretty rule) <+> "at" <+> PP.pretty source

empty :: StackTrace
empty = StackTrace mempty

null :: StackTrace -> Bool
null (StackTrace s) = L.null s
