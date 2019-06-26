-- | Source code and source code locations.
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Sources
    ( SourcePointer (..), _ReplInput, _FileInput, _CliInput, _TestInput
    , describeSourcePointer

    , Sources
    , empty
    , lookup
    , insert
    , delete

    , Handle
    , newHandle
    ) where

import           Control.Lens.TH              (makePrisms)
import           Data.Binary                  (Binary)
import           Data.Hashable                (Hashable)
import qualified Data.HashMap.Strict.Extended as HMS
import qualified Data.IORef                   as IORef
import           Data.Semigroup               (Semigroup)
import qualified Data.Text                    as T
import           GHC.Generics
import           Prelude                      hiding (lookup)

data SourcePointer
    = ReplInput Int T.Text
    | FileInput FilePath
    | CliInput
    | TestInput
    deriving (Eq, Generic, Ord, Show)

instance Binary SourcePointer
instance Hashable SourcePointer

$(makePrisms ''SourcePointer)

describeSourcePointer :: SourcePointer -> String
describeSourcePointer (ReplInput _ txt) = T.unpack txt
describeSourcePointer (FileInput p)     = p
describeSourcePointer CliInput          = "cli"
describeSourcePointer TestInput         = "tests"

newtype Sources = Sources
    { unSourceStore :: HMS.HashMap SourcePointer T.Text
    } deriving (Binary, Generic, Monoid, Semigroup)

empty :: Sources
empty = Sources HMS.empty

lookup :: SourcePointer -> Sources -> Maybe T.Text
lookup sp ss = HMS.lookup sp (unSourceStore ss)

insert :: SourcePointer -> T.Text -> Sources -> Sources
insert sp txt = Sources . HMS.insert sp txt . unSourceStore

delete :: SourcePointer -> Sources -> Sources
delete sp = Sources . HMS.delete sp . unSourceStore

type Handle = IORef.IORef Sources

newHandle :: IO Handle
newHandle = IORef.newIORef empty
