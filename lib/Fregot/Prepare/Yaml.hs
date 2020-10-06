{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
--------------------------------------------------------------------------------
-- | Convert a YAML document to a prepared rule.
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Prepare.Yaml
    ( loadYaml
    ) where

import           Control.Lens              (review, (^.))
import           Control.Monad             (forM)
import           Data.Bifunctor            (first)
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe)
import qualified Data.Scientific           as Scientific
import qualified Data.YAML                 as Yaml
import qualified Fregot.Error              as Error
import           Fregot.Lexer.Position     (Position (..))
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Prepare.BuildTree
import           Fregot.Prepare.Lens
import           Fregot.Prepare.Package    (PreparedRule)
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourcePointer, SourceSpan (..))
import qualified Fregot.Tree               as Tree

loadYaml
    :: PackageName
    -> SourcePointer
    -> BL.ByteString
    -> Either Error.Error (Tree.Tree PreparedRule)
loadYaml pkgname sourcePointer bytestring = first toError $ do
    node <- first (first mkSourceSpan) $ Yaml.decode1 bytestring
    case buildTree (fmap mkSourceSpan node) of
        BuildTree _ btree -> Right $ toTree pkgname btree
        BuildSingleton term ->
            Left (term ^. termAnn, "expected object at the top level")
  where
    mkSourceSpan :: Yaml.Pos -> SourceSpan
    mkSourceSpan ypos =
        let pos = Position (Yaml.posLine ypos) (Yaml.posColumn ypos + 1) in
        SourceSpan sourcePointer pos pos

    toError (loc, e) = Error.mkError "parse" loc "parse failed" $ PP.pretty e

buildTree
    :: Yaml.Node SourceSpan
    -> BuildTree
buildTree (Yaml.Mapping loc _ nodes) = fromMaybe
    (BuildSingleton $ ObjectT loc
        [ (toTerm (buildTree k), toTerm (buildTree v))
        | (k, v) <- Map.toList nodes
        ])
    (fmap (BuildTree loc) $ forM (Map.toList nodes) $ \(key, node) -> do
        (kloc, text) <- case key of
            Yaml.Scalar sl (Yaml.SStr t) -> pure (sl, t)
            _                            -> Nothing
        pure (kloc, mkVar text, buildTree node))
buildTree (Yaml.Anchor _ _ node) = buildTree node
buildTree (Yaml.Sequence loc _ nodes) = BuildSingleton . ArrayT loc $
    map (toTerm . buildTree) nodes
buildTree (Yaml.Scalar loc scalar) =
    BuildSingleton . ValueT loc . review valueToScalar $
    case scalar of
        Yaml.SNull        -> Null
        Yaml.SBool    b   -> Bool b
        Yaml.SFloat   f   -> Number (Scientific.fromFloatDigits f)
        Yaml.SInt     i   -> Number (fromIntegral i)
        Yaml.SUnknown _ _ -> Null
        Yaml.SStr     t   -> String t
