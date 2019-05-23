{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Repl.Breakpoint
    ( Suspension
    , Breakpoint (..)
    , breakpointFromText
    , qualifyBreakpoint
    , isBreakpoint
    ) where

import           Control.Lens              ((^.), (^?))
import           Control.Lens.Prism        (Prism', prism')
import           Data.Hashable             (Hashable)
import qualified Data.HashSet              as HS
import           Data.Maybe                (fromMaybe)
import qualified Data.Text                 as T
import qualified Fregot.Error.Stack        as Stack
import           Fregot.Names
import qualified Fregot.Sources            as Sources
import           Fregot.Sources.SourceSpan (SourceSpan)
import qualified Fregot.Sources.SourceSpan as SourceSpan
import           GHC.Generics              (Generic)
import           System.FilePath           (normalise)
import           Text.Read                 (readMaybe)

type Suspension = (SourceSpan, Stack.StackTrace)

data Breakpoint
    = NameBreakpoint   Name
    | SourceBreakpoint FilePath Int  -- Make sure to 'normalise' the file path!
    deriving (Eq, Generic)

instance Hashable Breakpoint

breakpointFromText :: Prism' T.Text Breakpoint
breakpointFromText = prism'
    (\case
        NameBreakpoint  n     -> nameToText n
        SourceBreakpoint fp l -> T.pack $ fp ++ ":" ++ show l)
    (\txt -> case T.split (== ':') txt of
        [path, line] -> SourceBreakpoint
            <$> pure (normalise $ T.unpack path)
            <*> readMaybe (T.unpack line)
        _ -> NameBreakpoint <$> nameFromText txt)

-- | We can only break on fully qualified names.  If the user entered a local
-- variable, we want to qualify it using the currently open package.
qualifyBreakpoint :: PackageName -> Breakpoint -> Breakpoint
qualifyBreakpoint pkg = \case
    NameBreakpoint (LocalName v) -> NameBreakpoint (QualifiedName pkg v)
    bpt                          -> bpt

isBreakpoint :: Suspension -> HS.HashSet Breakpoint -> Bool
isBreakpoint (sourcespan, stack) bkpnts = nameBreak || sourceBreak
  where
    nameBreak = case Stack.peek stack of
        Nothing                                -> False
        Just (Stack.RuleStackFrame name _)     ->
            NameBreakpoint name `HS.member` bkpnts
        Just (Stack.FunctionStackFrame name _) ->
            NameBreakpoint name `HS.member` bkpnts

    sourceBreak = fromMaybe False $ do
        let line = sourcespan ^. SourceSpan.start . SourceSpan.line
        path <- sourcespan ^? SourceSpan.sourcePointer . Sources._FileInput
        return $ SourceBreakpoint (normalise path) line `HS.member` bkpnts
