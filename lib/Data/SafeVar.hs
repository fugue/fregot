{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Data.SafeVar
    ( Statement (..), statementExpr, statementIn, statementOut

    , Key
    , Node (..)
    , Graph
    , toGraph
    ) where

import           Control.Lens    (foldOf, (^.))
import           Control.Lens.TH (makeLenses)
import           Data.Bifunctor  (second)
import           Data.Hashable   (Hashable)
import qualified Data.HashSet    as HS
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe      (fromMaybe, mapMaybe)

data Statement s v = Statement
    { _statementExpr :: !s
    , _statementIn   :: !(HS.HashSet v)
    , _statementOut  :: !(HS.HashSet v)
    } deriving (Eq, Show)

$(makeLenses ''Statement)

type Key = Int

data Node s v
    = StatementNode (Statement s v)
    | VarNode v
    deriving (Show)

type Graph s v = Map Key (Node s v, [Key])

toGraph
    :: forall s v. (Eq v, Hashable v, Ord v)
    => [Statement s v] -> Graph s v
toGraph statements =
    Map.mapWithKey (\k x -> (x, fromMaybe [] (Map.lookup k edges))) nodes
  where
    allVariables :: HS.HashSet v
    allVariables =
        foldOf (traverse . statementIn) statements <>
        foldOf (traverse . statementOut) statements

    idxVariables :: Map v Key
    idxVariables = Map.fromList $
        zip (HS.toList allVariables) [length idxStatements ..]

    idxStatements :: [(Key, Statement s v)]
    idxStatements = zip [0 ..] statements

    toKeyList :: HS.HashSet v -> [Key]
    toKeyList = mapMaybe (\v -> Map.lookup v idxVariables) . HS.toList

    nodes :: Map Key (Node s v)
    nodes = Map.fromList $
        [(key, VarNode var) | (var, key) <- Map.toList idxVariables] ++
        map (second StatementNode) idxStatements

    edges :: Map Key [Key]
    edges = Map.fromListWith (++) $ do
        (k, s) <- idxStatements
        let oute = toKeyList (s ^. statementOut)
            ine  = toKeyList (s ^. statementIn)
        (k, ine) : [(o, [k]) | o <- oute]
