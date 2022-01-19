{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
module Fregot.Types.Internal
    ( StaticDynamic (..)
    , sdLookup
    , sdValues

    , Elem (..), _String, _Number, _Boolean, _Null, _Scalar, _Object, _Array
    , _Set
    , Type (..)

      -- Queries
    , subsetOf, (⊆)
    , union, (∪)
    , unions
    , intersection, (∩)

      -- Utilities
    , singleton
    , void
    , any
    , unknown
    , boolean
    , number
    , string
    , null
    , object
    , objectOf
    , setOf
    , array
    , arrayOf
    , collectionOf
    , scalarType

      -- Deconstruction
    , unArray
    ) where

import           Control.Applicative ((<|>))
import           Control.Lens        (Prism', prism', review, (^?))
import           Control.Lens.TH     (makePrisms)
import           Control.Monad       (forM)
import           Data.Foldable       (toList)
import           Data.Functor        (($>))
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet        as HS
import           Data.Hashable       (Hashable)
import qualified Data.Kleene         as K
import           Data.List           (foldl', intersperse, sort)
import           Data.Maybe          (catMaybes, mapMaybe, maybeToList)
import qualified Fregot.Prepare.Ast  as Ast
import qualified Fregot.PrettyPrint  as PP
import           GHC.Generics        (Generic)
import           Prelude             hiding (any, null)
import qualified Prelude             as Prelude

-- | A structure that is partially statically typed, and partially dynamically
-- typed.  Used for objects and arrays.
data StaticDynamic k ty = StaticDynamic
    { sdStatic  :: HMS.HashMap k ty
    , sdDynamic :: Maybe (ty, ty)
    } deriving (Eq, Generic, Ord, Show)

instance (Hashable k, Hashable ty) => Hashable (StaticDynamic k ty)

sdLookup :: (Eq k, Hashable k) => k -> StaticDynamic k ty -> Maybe ty
sdLookup k sd = HMS.lookup k (sdStatic sd) <|> fmap snd (sdDynamic sd)

sdValues :: StaticDynamic k ty -> [ty]
sdValues sd =
    fmap snd (HMS.toList $ sdStatic sd) ++
    fmap snd (maybeToList $ sdDynamic sd)

data Elem ty
    = String
    | Number
    | Boolean
    | Null
    | Scalar Ast.Scalar
    | Object (StaticDynamic Ast.Scalar ty)
    | Array  (StaticDynamic Int ty)
    | Set    ty
    deriving (Eq, Generic, Ord, Show)

instance Hashable ty => Hashable (Elem ty)

data Type
    = Universe
    | Unknown
    | Union !(HS.HashSet (Elem Type))
    deriving (Eq, Generic, Ord, Show)

instance Hashable Type

$(makePrisms ''Elem)


--------------------------------------------------------------------------------
-- Pretty printing.

instance (PP.Pretty PP.Sem k, PP.Pretty PP.Sem ty) =>
        PP.Pretty PP.Sem (StaticDynamic k ty) where
    pretty StaticDynamic {..} = PP.object $
        [(PP.pretty' k, v) | (k, v) <- HMS.toList sdStatic] ++
        [ (PP.pretty dynKey, dynVal)
        | (dynKey, dynVal) <- maybeToList sdDynamic
        ]

instance PP.Pretty PP.Sem ty => PP.Pretty PP.Sem (Elem ty) where
    pretty = \case
        Number        -> PP.keyword "number"
        String        -> PP.keyword "string"
        Boolean       -> PP.keyword "boolean"
        Null          -> PP.keyword "null"
        Scalar s      -> PP.pretty s
        Object x      -> PP.keyword "object" <> PP.pretty x
        Array  x      -> PP.keyword "array" <> PP.pretty x
        Set    x      -> PP.keyword "set" <>
                            PP.punctuation "{" <> PP.pretty x <>
                            PP.punctuation "}"

instance PP.Pretty PP.Sem Type where
    pretty = \case
        Universe              -> PP.keyword "any"
        Unknown               -> PP.keyword "unknown"
        Union es | HS.null es -> PP.keyword "empty"
        Union es              -> mconcat $ intersperse (PP.punctuation "|") .
            map PP.pretty . sort $ HS.toList es


--------------------------------------------------------------------------------


scalarElem :: Ast.Scalar -> Elem ty
scalarElem (Ast.String  _) = String
scalarElem (Ast.Number  _) = Number
scalarElem (Ast.Bool _)    = Boolean
scalarElem Ast.Null        = Null

objectSubsetOf
    :: (Eq k, Hashable k)
    => (k -> ty) -> (ty -> ty -> K.Ternary)
    -> StaticDynamic k ty -> StaticDynamic k ty -> K.Ternary
objectSubsetOf keyTy sub l r =
    -- 1. All static keys in `r` must be present in the static keys of `l`.
    (K.fromBool $ Prelude.null $ sdStatic r `HMS.difference` sdStatic l) K.&&
    -- 2. All static keys in `l` must be either be subsets of static keys in
    -- `r`, or be a subset of the dynamic part of `r`.
    (K.all
        (\(k, lv) -> case HMS.lookup k (sdStatic r) of
            Just rv -> lv `sub` rv
            Nothing -> case sdDynamic r of
                Nothing         -> K.False
                Just (rdk, rdv) -> keyTy k `sub` rdk K.&& lv `sub` rdv)
        (HMS.toList (sdStatic l))) K.&&
    -- 3. If `l` has a dynamic part, it must be a subset of `r`s dynamic part.
    -- If it does not have a dynamic part, `r` should not have one either.
    (case (sdDynamic l, sdDynamic r) of
        (Nothing,       Nothing)       -> K.True
        (Nothing,       Just _)        -> K.True
        (Just _,        Nothing)       -> K.False
        (Just (lk, lv), Just (rk, rv)) -> sub lk rk K.&& sub lv rv)

elemSubsetOf :: Elem Type -> Elem Type -> K.Ternary
elemSubsetOf x y
    | x == y = K.True
elemSubsetOf (Scalar s) y
    | scalarElem s == y = K.True
elemSubsetOf (Object x) (Object y) =
    objectSubsetOf scalarType (⊆) x y
elemSubsetOf (Array x) (Array y) =
    objectSubsetOf (const number) (⊆) x y
elemSubsetOf (Set x) (Set y) =
    subsetOf x y
elemSubsetOf _ _ = K.False

subsetOf :: Type -> Type -> K.Ternary
subsetOf _         Universe  = K.True
subsetOf Universe  (Union _) = K.False
subsetOf Unknown   _         = K.Unknown
subsetOf _         Unknown   = K.Unknown
subsetOf (Union l) (Union r) = K.all (\e -> K.any (e `elemSubsetOf`) r) l

(⊆) :: Type -> Type -> K.Ternary
(⊆) = subsetOf

union :: Type -> Type -> Type
union Universe _          = Universe
union _        Universe   = Universe
union Unknown  _          = Unknown
union _        Unknown    = Unknown
union (Union l) (Union r) = Union $ HS.fromList $ foldl' insert (HS.toList r) l
  where
    insert :: [Elem Type] -> Elem Type -> [Elem Type]
    insert = work []

    work acc []       elm = elm : acc
    work acc (e : es) elm
        | K.isTrue (e `elemSubsetOf` elm) = work acc es elm
        | K.isTrue (elm `elemSubsetOf` e) = acc ++ e : es
        | otherwise                       = work (e : acc) es elm

unions :: [Type] -> Type
unions = foldl' union void

(∪) :: Type -> Type -> Type
(∪) = union

unionStaticDynamic
    :: (Eq k, Hashable k)
    => StaticDynamic k Type
    -> StaticDynamic k Type
    -> StaticDynamic k Type
unionStaticDynamic (StaticDynamic s0 d0) (StaticDynamic s1 d1) = StaticDynamic
    (HMS.unionWith union s0 s1)
    (case (d0, d1) of
        (Nothing, x)                   -> x
        (x, Nothing)                   -> x
        (Just (k0, v0), Just (k1, v1)) -> Just (union k0 k1, union v0 v1))

objectIntersection
    :: (Eq k, Hashable k)
    => (k -> Type)
    -> StaticDynamic k Type -> StaticDynamic k Type
    -> Maybe (StaticDynamic k Type)
objectIntersection keyTy l r = do
    static <- mbStatic
    pure $ StaticDynamic static dynamic
  where
    keys = HS.union (HMS.keysSet (sdStatic l)) (HMS.keysSet (sdStatic r))
    mbStatic = fmap HMS.fromList $ forM (HS.toList keys) $ \k ->
        case (HMS.lookup k (sdStatic l), HMS.lookup k (sdStatic r)) of
            (Nothing, Nothing) -> Nothing
            (Just v,  Nothing) -> case sdDynamic r of
                Nothing           -> Nothing
                Just (kdyn, wdyn) -> (,)
                    <$> (intersectionMaybe (keyTy k) kdyn $> k)
                    <*> intersectionMaybe v wdyn
            (Nothing, Just w)  -> case sdDynamic l of
                Nothing           -> Nothing
                Just (kdyn, vdyn) -> (,)
                    <$> (intersectionMaybe (keyTy k) kdyn $> k)
                    <*> intersectionMaybe w vdyn
            (Just v,  Just w)  -> (,) k <$> intersectionMaybe v w

    dynamic = case (sdDynamic l, sdDynamic r) of
        (Nothing,       Nothing)       -> Nothing
        (Nothing,       Just _)        -> Nothing
        (Just _,        Nothing)       -> Nothing
        (Just (lk, lv), Just (rk, rv)) ->
            (,) <$> intersectionMaybe lk rk <*> intersectionMaybe lv rv

elemIntersection :: Elem Type -> Elem Type -> Maybe (Elem Type)
elemIntersection x y                   | x == y = Just x
elemIntersection (Scalar s) y          | scalarElem s == y = Just (Scalar s)
elemIntersection x (Scalar s)          | scalarElem s == x = Just (Scalar s)
elemIntersection (Object x) (Object y) = Object <$> objectIntersection scalarType x y
elemIntersection (Set x) (Set y)       = Set <$> intersectionMaybe x y
elemIntersection (Array x) (Array y)   = Array <$> objectIntersection (const number) x y
elemIntersection _ _                   = Nothing

intersectionMaybe :: Type -> Type -> Maybe Type
intersectionMaybe l r =
    let lr = intersection l r in if lr == void then Nothing else Just lr

intersection :: Type -> Type -> Type
intersection Universe  r         = r
intersection l         Universe  = l
intersection Unknown   _         = Unknown
intersection _         Unknown   = Unknown
intersection (Union l) (Union r) = Union $ HS.fromList $ catMaybes $ do
    x <- HS.toList l
    y <- HS.toList r
    pure $ elemIntersection x y

(∩) :: Type -> Type -> Type
(∩) = intersection


--------------------------------------------------------------------------------
-- | Constructing types.

singleton :: Prism' Type (Elem Type)
singleton = prism'
    (Union . HS.singleton)
    (\case
        Universe  -> Nothing
        Unknown   -> Nothing
        Union set -> case HS.toList set of
            [single] -> Just single
            _        -> Nothing)

scalarType :: Ast.Scalar -> Type
scalarType = review singleton . scalarElem

void :: Type
void = Union HS.empty

any :: Type
any = Universe

unknown :: Type
unknown = Unknown

boolean :: Type
boolean = review singleton Boolean

number :: Type
number = review singleton Number

string :: Type
string = review singleton String

null :: Type
null = review singleton Null

object :: StaticDynamic Ast.Scalar Type -> Type
object = review singleton . Object

objectOf :: Type -> Type -> Type
objectOf k v = object . StaticDynamic HMS.empty $ Just (k, v)

array :: StaticDynamic Int Type -> Type
array = review singleton . Array

arrayOf :: Type -> Type
arrayOf ty = array . StaticDynamic HMS.empty $ Just (number, ty)

setOf :: Type -> Type
setOf = review singleton . Set

collectionOf :: Type -> Type
collectionOf x = arrayOf x ∪ setOf x ∪ objectOf any x

-- | Deconstruct an array type into it's element type.
unArray :: Type -> Maybe (StaticDynamic Int Type)
unArray (Union tys) = case mapMaybe (^? _Array) (toList tys) of
    sd : sds -> Just $ foldl' unionStaticDynamic sd sds
    _        -> Nothing
unArray ty          = Just $ StaticDynamic mempty $ Just (number, ty)
