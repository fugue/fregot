module Data.Unique
    ( Unique
    , HasUnique (..)

    , GlobalUniqueGen
    , newGlobalUniqueGen
    , getGlobalUnique

    , StableUniqueGen
    , newStableUniqueGen
    , getStableUnique
    , getFreshUnique
    ) where

import           Data.Unique.Internal
