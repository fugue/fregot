--------------------------------------------------------------------------------
-- | Convert a JSON document to a prepared rule.
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Prepare.Json
    ( loadJson
    ) where

import           Control.Applicative       ((<|>))
import           Control.Lens              (review, (^.))
import           Control.Monad.Parachute
import qualified Data.Text                 as T
import qualified Fregot.Error              as Error
import           Fregot.Parser             (FregotParser, lexAndParse)
import qualified Fregot.Parser.Internal    as P
import qualified Fregot.Parser.Sugar       as P
import           Fregot.Prepare.Ast
import           Fregot.Prepare.BuildTree
import           Fregot.Prepare.Lens       (termAnn, termToScalar)
import           Fregot.Prepare.Package    (PreparedRule)
import           Fregot.Sources.SourceSpan (SourcePointer)
import qualified Fregot.Tree               as Tree
import qualified Text.Parsec.Extended      as Parsec

loadJson
    :: Monad m
    => SourcePointer -> T.Text
    -> ParachuteT Error.Error m (Tree.Tree PreparedRule)
loadJson sourcePointer text = do
    tree <- lexAndParse parseJson sourcePointer text
    case tree of
        BuildTree _ btree -> pure $ toTree mempty btree
        BuildSingleton term -> fatal $ Error.mkError
            "parse" (term ^. termAnn) "parse failed"
            "expected object at the top level"

parseJson :: FregotParser BuildTree
parseJson =
    (P.withSourceSpan $ do
        scalar <- P.scalar
        pure $ \loc -> BuildSingleton $ review termToScalar (loc, scalar)) <|>
    (P.withSourceSpan $ do
        arr <- P.array parseJson
        pure $ \loc -> BuildSingleton $ ArrayT loc $ map toTerm arr) <|>
    (P.withSourceSpan $ do
        let key = P.withSourceSpan $ P.string >>= \t -> pure (\l -> (l, t))
        objOrSet <- P.objectOrSet key parseJson
        case objOrSet of
            Right _  -> Parsec.unexpected "unexpected set in plain JSON"
            Left obj -> pure $ \loc -> BuildTree loc $
                [(l, mkVar k, t) | ((l, k), t) <- obj])
