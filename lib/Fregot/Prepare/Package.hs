{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Prepare.Package
    ( PreparedRule
    , PreparedTree
    , prepareModules
    , prepareModule
    , mergeTree
    , mergeTrees
    ) where

import           Control.Lens              (at, forOf, review, (^.))
import           Control.Monad             (foldM, (>=>))
import           Control.Monad.Parachute
import           Fregot.Error              (Error)
import           Fregot.Names
import           Fregot.Names.Imports      (gatherImports)
import           Fregot.Prepare
import           Fregot.Prepare.Ast
import           Fregot.Sources.SourceSpan (SourceSpan)
import qualified Fregot.Sugar              as Sugar
import qualified Fregot.Tree               as Tree
import           Prelude                   hiding (head, lookup)

type PreparedRule = Rule () SourceSpan
type PreparedTree = Tree.Tree PreparedRule

prepareModules
    :: Monad m
    => Sugar.Modules SourceSpan Name
    -> ParachuteT Error m PreparedTree
prepareModules = mapM prepareModule >=> mergeTrees

prepareModule
    :: Monad m
    => Sugar.Module SourceSpan Name
    -> ParachuteT Error m PreparedTree
prepareModule modul = foldM
    (\tree rule -> do
        let key :: Tree.Key
            key = review Tree.qualifiedVarFromKey
                ( modul ^. Sugar.modulePackage
                , rule ^. Sugar.ruleHead . Sugar.ruleName
                )

        prule <- prepareRule (modul ^. Sugar.modulePackage) imports rule
        forOf (at key) tree $ \case
            Nothing -> pure (Just prule)
            Just prule' -> Just <$> mergeRules prule prule')
    Tree.empty
    (modul ^. Sugar.modulePolicy)
  where
    imports = gatherImports (modul ^. Sugar.moduleImports)

mergeTree
    :: Monad m
    => PreparedTree
    -> PreparedTree
    -> ParachuteT Error m PreparedTree
mergeTree = Tree.unionWithA mergeRules

mergeTrees
    :: Monad m
    => [PreparedTree]
    -> ParachuteT Error m PreparedTree
mergeTrees = foldM mergeTree Tree.empty
