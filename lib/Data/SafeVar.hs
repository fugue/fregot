{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Data.SafeVar
    ( Statement (..), statementExpr, statementIn, statementOut

    , Key
    , Node (..)
    , toGraph
    ) where

import           Control.Lens        (foldOf, (^.))
import           Control.Lens.TH     (makeLenses)
import           Data.Bifunctor      (second)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet        as HS
import           Data.Maybe          (fromMaybe, mapMaybe)

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

toGraph
    :: forall s v. (Eq v, Hashable v)
    => [Statement s v] -> [(Node s v, Key, [Key])]
toGraph statements =
    [ (node, key, fromMaybe [] (HMS.lookup key edges))
    | (key, node) <- nodes
    ]
  where
    allVariables :: HS.HashSet v
    allVariables =
        foldOf (traverse . statementIn) statements <>
        foldOf (traverse . statementOut) statements

    idxVariables :: HMS.HashMap v Key
    idxVariables = HMS.fromList $ zip (HS.toList allVariables) [0 ..]

    idxStatements :: [(Key, Statement s v)]
    idxStatements = zip [HS.size allVariables ..] statements

    toKeyList :: HS.HashSet v -> [Key]
    toKeyList = mapMaybe (\v -> HMS.lookup v idxVariables) . HS.toList

    nodes :: [(Key, Node s v)]
    nodes =
        [(key, VarNode var) | (var, key) <- HMS.toList idxVariables] ++
        map (second StatementNode) idxStatements

    edges :: HMS.HashMap Key [Key]
    edges = HMS.fromListWith (++) $ do
        (k, s) <- idxStatements
        let oute = toKeyList (s ^. statementOut)
            ine  = toKeyList (s ^. statementIn)
        (k, oute) : [(i, [k]) | i <- ine]
