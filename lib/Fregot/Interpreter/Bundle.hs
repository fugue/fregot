{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Fregot.Interpreter.Bundle
    ( ModuleBatch
    , Bundle (..), bundleSources, bundleModules
    ) where

import           Control.Lens              ((^.))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad             (when)
import           Data.Binary               (Binary (..))
import qualified Data.HashMap.Strict       as HMS
import           Data.Version              (showVersion)
import           Fregot.Names              (Var)
import           Fregot.Sources            (Sources)
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Fregot.Sugar              (PackageName)
import qualified Fregot.Sugar              as Sugar
import qualified Fregot.Version            as Fregot

-- | The modules that make up a package.
type ModuleBatch = Sugar.Modules SourceSpan Var

data Bundle = Bundle
    { _bundleSources :: !Sources
    , _bundleModules :: !(HMS.HashMap PackageName ModuleBatch)
    }

$(makeLenses ''Bundle)

instance Binary Bundle where
    put bundle = do
        put Fregot.version
        put (bundle ^. bundleSources)
        put (bundle ^. bundleModules)

    get = do
        bversion <- get
        when (bversion /= Fregot.version) $ fail $
            "version mismatch: bundle=" ++ showVersion Fregot.version ++
            ", fregot=" ++ showVersion bversion
        _bundleSources <- get
        _bundleModules <- get
        return Bundle {..}
