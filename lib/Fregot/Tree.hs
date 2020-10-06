{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Fregot.Tree
    ( Key
    , qualifiedVarFromKey
    , packageNameFromKey

    , Tree (..)

    , empty
    , singleton
    , parent
    , insert
    , prefix
    , fromList

    , null
    , member
    , lookup
    , keys
    , toList

    , root
    , children
    , descendant

    , union
    , unionWithA
    , difference
    , filterWithKey
    , alterF
    ) where

import           Control.Lens         ((<&>))
import           Control.Lens.At      (At (..), Index, IxValue, Ixed (..))
import           Control.Monad        (mplus)
import qualified Data.HashMap.Strict  as HMS
import qualified Data.List            as L
import           Data.Maybe           (isJust, maybeToList)
import qualified Data.Vector.Extended as V
import           Fregot.Names
import qualified Fregot.PrettyPrint   as PP
import           Prelude              hiding (lookup, null)

data Tree a = Tree !(Maybe a) !(HMS.HashMap Var (Tree a))
    deriving (Foldable, Functor, Traversable, Show)

instance PP.Pretty ann a => PP.Pretty ann (Tree a) where
    pretty (Tree mbVal cs) =
        (maybe mempty (Just . PP.pretty) mbVal) PP.?<$$> PP.vcat
        [ "@" <> PP.pretty v PP.<$$> PP.indent 2 (PP.pretty t)
        | (v, t) <- HMS.toList cs
        ]

empty :: Tree a
empty = Tree Nothing HMS.empty

singleton :: Key -> a -> Tree a
singleton key x = prefix key (Tree (Just x) HMS.empty)

parent :: [(Var, Tree a)] -> Tree a
parent = Tree Nothing . HMS.fromList

insert :: Key -> a -> Tree a -> Tree a
insert (Key key) x (Tree mbX cs) = case V.uncons key of
    Nothing      -> Tree (Just x) cs
    Just (k, ks) -> Tree mbX $ case HMS.lookup k cs of
        Nothing -> HMS.insert k (singleton (Key ks) x) cs
        Just ct -> HMS.insert k (insert (Key ks) x ct) cs

-- | Prefix an entire tree with a key.
prefix :: Key -> Tree a -> Tree a
prefix (Key key) t = case V.uncons key of
    Nothing      -> t
    Just (k, ks) -> Tree Nothing (HMS.singleton k (prefix (Key ks) t))

fromList :: [(Key, a)] -> Tree a
fromList = L.foldl' (\acc (k, x) -> insert k x acc) empty

null :: Tree a -> Bool
null (Tree Nothing cs) = HMS.null cs  -- Works because we always clean children.
null (Tree _       _)  = False

member :: Key -> Tree a -> Bool
member key = isJust . lookup key
{-# INLINE member #-}

lookup :: Key -> Tree a -> Maybe a
lookup key = maybe Nothing root . descendant key
{-# INLINE lookup #-}

toList :: Tree a -> [(Key, a)]
toList (Tree mbX cs) =
    [ (mempty, x) | x <- maybeToList mbX] ++
    [ (Key (V.singleton cv) <> k, y)
    | (cv, ct) <- HMS.toList cs, (k, y) <- toList ct
    ]

keys :: Tree a -> [Key]
keys = map fst . toList
{-# INLINE keys #-}

root :: Tree a -> Maybe a
root (Tree mbX _) = mbX
{-# INLINE root #-}

children :: Tree a -> [(Var, Tree a)]
children (Tree _ cs) = HMS.toList cs
{-# INLINE children #-}

-- | Get the descendant with the given key; if it exists.
descendant :: Key -> Tree a -> Maybe (Tree a)
descendant (Key key) tree =
    V.foldM (\(Tree _ cs) k -> HMS.lookup k cs) tree key
{-# INLINE descendant #-}

union :: Tree a -> Tree a -> Tree a
union (Tree mbX csl) (Tree mbY csr) =
    Tree (mbX `mplus` mbY) (HMS.unionWith union csl csr)

-- | For efficiency reasons, the bigger tree should be passed as the left side
-- argument.
unionWithA :: Applicative f => (a -> a -> f a) -> Tree a -> Tree a -> f (Tree a)
unionWithA f (Tree mbX csl) (Tree mbY csr) =
    Tree <$> mbZ <*> (fmap (`unionLeft` csr) mergedLeft)
  where
    -- Merged values.
    mbZ = case (mbX, mbY) of
        (Just x,  Just y)  -> Just <$> f x y
        (Nothing, _)       -> pure mbY
        (_,       Nothing) -> pure mbX

    -- Merge keys that are in the left children.  This should be the majority
    -- of keys.
    mergedLeft = HMS.traverseWithKey
        (\k lt -> do
            case HMS.lookup k csr of
                Nothing -> pure lt
                Just rt -> unionWithA f lt rt) csl

    -- Add keys that appear only in the right children.
    unionLeft = HMS.foldlWithKey'
        (\acc k x -> if k `HMS.member` acc then acc else HMS.insert k x acc)

difference :: Tree a -> Tree b -> Tree a
difference (Tree mbX csl) (Tree mbY csr) =
    Tree
        (if isJust mbY then Nothing else mbX)
        (HMS.mapMaybeWithKey (\k cl ->
            case HMS.lookup k csr of
                Nothing -> Just cl
                Just cr ->
                    let t' = difference cl cr in
                    if null t' then Nothing else Just t')
            csl)

filterWithKey :: (Key -> a -> Bool) -> Tree a -> Tree a
filterWithKey p = go mempty
  where
    -- Here, we want to do a bottom-up traversal and remove any child trees that
    -- have become empty.
    go key (Tree mbX cs) = Tree
        (case mbX of
            Nothing -> Nothing
            Just x  -> if p key x then mbX else Nothing)
        (HMS.filter (not . null) $
            HMS.mapWithKey (\cv ct -> go (key <> Key (V.singleton cv)) ct) cs)

alterF
    :: Functor f
    => (Maybe a -> f (Maybe a)) -> Key -> Tree a -> f (Tree a)
alterF f (Key key) (Tree mbX cs) = case V.uncons key of
    Nothing      -> f mbX <&> \mbX' -> Tree mbX' cs
    Just (k, ks) ->
        HMS.alterF (\case
            Nothing -> fmap (singleton (Key ks)) <$> f Nothing
            Just t  -> Just <$> (alterF f (Key ks) t)) k cs <&> \cs' ->
        Tree mbX (HMS.filter (not . null) cs')

type instance Index   (Tree a) = Key
type instance IxValue (Tree a) = a

instance Ixed (Tree a) where
    ix k f t = case lookup k t of
        Nothing -> pure t
        Just v  -> f v <&> \v' -> insert k v' t

instance At (Tree a) where
    at k f = alterF f k
