-- | Source code and source code locations.
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fregot.Sources
    ( SourcePointer (..)
    , describeSourcePointer

    , Sources
    , empty
    , lookup
    , insert
    , delete
    ) where

import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HMS
import           Data.Semigroup      (Semigroup)
import qualified Data.Text           as T
import           GHC.Generics
import           Prelude             hiding (lookup)

data SourcePointer
    = ReplInput Int T.Text
    deriving (Eq, Generic, Ord, Show)

instance Hashable SourcePointer

describeSourcePointer :: SourcePointer -> String
describeSourcePointer (ReplInput _ txt) = T.unpack txt

newtype Sources = Sources
    { unSourceStore :: HMS.HashMap SourcePointer T.Text
    } deriving (Generic, Monoid, Semigroup)

empty :: Sources
empty = Sources HMS.empty

lookup :: SourcePointer -> Sources -> Maybe T.Text
lookup sp ss = HMS.lookup sp (unSourceStore ss)

insert :: SourcePointer -> T.Text -> Sources -> Sources
insert sp txt = Sources . HMS.insert sp txt . unSourceStore

delete :: SourcePointer -> Sources -> Sources
delete sp = Sources . HMS.delete sp . unSourceStore
