--------------------------------------------------------------------------------
-- | Convert a YAML document to a prepared rule.
module Fregot.Prepare.Yaml
    ( loadYaml
    ) where

import           Control.Lens              (review, (^.))
import           Control.Monad             (forM)
import           Data.Bifunctor            (first)
import qualified Data.ByteString.Lazy      as BL
import qualified Data.List                 as L
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe)
import qualified Data.Scientific           as Scientific
import qualified Data.YAML                 as Yaml
import           Fregot.Lexer.Position     (Position (..))
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens
import           Fregot.Prepare.Package    (PreparedRule)
import           Fregot.Sources.SourceSpan (SourcePointer, SourceSpan (..))
import qualified Fregot.Tree               as Tree

loadYaml
    :: SourcePointer
    -> BL.ByteString
    -> Either (SourceSpan, String) (Tree.Tree PreparedRule)
loadYaml sourcePointer bytestring = do
    node <- first (first mkSourceSpan) $ Yaml.decode1 bytestring
    case buildTree (fmap mkSourceSpan node) of
        BuildTree _ btree -> Right $ toTree mempty btree
        BuildSingleton term ->
            Left (term ^. termAnn, "expected object at the top level")
  where
    mkSourceSpan :: Yaml.Pos -> SourceSpan
    mkSourceSpan ypos =
        let pos = Position (Yaml.posLine ypos) (Yaml.posColumn ypos + 1) in
        SourceSpan sourcePointer pos pos

data BuildTree
    = BuildSingleton (Term SourceSpan)
    | BuildTree      SourceSpan [(SourceSpan, Var, BuildTree)]

toTree
    :: PackageName -> [(SourceSpan, Var, BuildTree)] -> Tree.Tree PreparedRule
toTree pkgname = L.foldl' Tree.union Tree.empty . map toRuleOrTree
  where
    toRuleOrTree (loc, var, BuildSingleton t) = Tree.singleton
        (review varFromKey var)
        Rule
            { _rulePackage = pkgname
            , _ruleName    = var
            , _ruleKey     = review qualifiedVarFromKey (pkgname, var)
            , _ruleAnn     = loc
            , _ruleKind    = CompleteRule
            , _ruleInfo    = ()
            , _ruleDefault = Nothing
            , _ruleDefs    = pure RuleDefinition
                { _ruleDefName    = var
                , _ruleDefImports = mempty
                , _ruleDefAnn     = loc
                , _ruleArgs       = Nothing
                , _ruleIndex      = Nothing
                , _ruleValue      = Just t
                , _ruleBodies     = []
                , _ruleElses      = []
                }
            }

    toRuleOrTree (_, var, BuildTree _ t) =
        Tree.parent [(var, toTree (pkgname <> mkPackageName [unVar var]) t)]

toTerm :: BuildTree -> Term SourceSpan
toTerm (BuildSingleton t) = t
toTerm (BuildTree loc children) = ObjectT loc $
    [ (ScalarT l (String $ unVar v), toTerm child)
    | (l, v, child) <- children
    ]

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
buildTree (Yaml.Scalar loc scalar) = BuildSingleton . ScalarT loc $
    case scalar of
        Yaml.SNull        -> Null
        Yaml.SBool    b   -> Bool b
        Yaml.SFloat   f   -> Number (Scientific.fromFloatDigits f)
        Yaml.SInt     i   -> Number (fromIntegral i)
        Yaml.SUnknown _ _ -> Null
        Yaml.SStr     t   -> String t
