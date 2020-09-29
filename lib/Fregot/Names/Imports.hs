{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

A more convenient representation of imports, collected under their aliases.
-}
module Fregot.Names.Imports
    ( Imports
    , gatherImports
    ) where

import           Control.Applicative ((<|>))
import           Control.Lens        ((^.))
import qualified Data.HashMap.Strict as HMS
import qualified Data.List.Extended  as L
import           Data.Maybe          (mapMaybe)
import           Fregot.Names
import qualified Fregot.Sugar        as Sugar

type Imports a = HMS.HashMap Var (a, Sugar.ImportGut)

gatherImports :: [Sugar.Import a] -> Imports a
gatherImports =
    HMS.fromList . mapMaybe gatherImport
  where
    gatherImport imp = do
        alias <- importAlias imp
        return $ (alias, (imp ^. Sugar.importAnn, imp ^. Sugar.importGut))

    importAlias imp =
        (imp ^. Sugar.importAs) <|>
        (case imp ^. Sugar.importGut of
            Sugar.ImportData  p -> mkVar <$> L.maybeLast (unPackageName p)
            Sugar.ImportInput p -> mkVar <$> L.maybeLast (unPackageName p))
