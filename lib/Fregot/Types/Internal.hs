{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
module Fregot.Types.Internal
    ( Object (..)
    , Elem (..), _String, _Number, _Boolean, _Null, _Scalar, _Object, _Array
    , _Set
    , Type (..)
    , union, (∪)
    , unions

      -- Queries
    , subsetOf, (⊆)

      -- Utilities
    , singleton
    , empty
    , any
    , unknown
    , boolean
    , number
    , string
    , null
    , object
    , objectOf
    , setOf
    , arrayOf
    , collectionOf
    , scalarType
    ) where

import           Control.Lens        (Prism', prism', review)
import           Control.Lens.TH     (makePrisms)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet        as HS
import qualified Data.Kleene         as K
import           Data.List           (foldl', intersperse)
import           Data.Maybe          (maybeToList)
import qualified Fregot.Prepare.Ast  as Ast
import qualified Fregot.PrettyPrint  as PP
import           GHC.Generics        (Generic)
import           Prelude             hiding (any, null)
import qualified Prelude             as Prelude

data Object k ty = Obj
    { objStatic  :: HMS.HashMap k ty
    , objDynamic :: Maybe (ty, ty)
    } deriving (Eq, Generic, Show)

instance (Hashable k, Hashable ty) => Hashable (Object k ty)

data Elem ty
    = String
    | Number
    | Boolean
    | Null
    | Scalar Ast.Scalar
    | Object (Object Ast.Scalar ty)
    -- | Array  (Object Int ty)
    | Array  ty
    | Set    ty
    deriving (Eq, Generic, Show)

instance Hashable ty => Hashable (Elem ty)

data Type
    = Universe
    | Unknown
    | Union !(HS.HashSet (Elem Type))
    deriving (Eq, Generic, Show)

instance Hashable Type

$(makePrisms ''Elem)


--------------------------------------------------------------------------------
-- Pretty printing.

instance (PP.Pretty PP.Sem k, PP.Pretty PP.Sem ty) =>
        PP.Pretty PP.Sem (Object k ty) where
    pretty Obj {..} = PP.object $
        [(PP.pretty' k, v) | (k, v) <- HMS.toList objStatic] ++
        [ (PP.pretty dynKey, dynVal)
        | (dynKey, dynVal) <- maybeToList objDynamic
        ]

instance PP.Pretty PP.Sem ty => PP.Pretty PP.Sem (Elem ty) where
    pretty = \case
        Number        -> PP.keyword "number"
        String        -> PP.keyword "string"
        Boolean       -> PP.keyword "boolean"
        Null          -> PP.keyword "null"
        Scalar s      -> PP.pretty s
        Object x      -> PP.keyword "object" <> PP.pretty x
        -- Array  x      -> PP.keyword "array" <> PP.pretty x
        Array  x      -> PP.keyword "array" <>
                            PP.punctuation "{" <> PP.pretty x <>
                            PP.punctuation "}"
        Set    x      -> PP.keyword "set" <>
                            PP.punctuation "{" <> PP.pretty x <>
                            PP.punctuation "}"

instance PP.Pretty PP.Sem Type where
    pretty = \case
        Universe              -> PP.keyword "any"
        Unknown               -> PP.keyword "unknown"
        Union es | HS.null es -> PP.keyword "empty"
        Union es              -> mconcat $
            intersperse (PP.punctuation "|") (map PP.pretty $ HS.toList es)


--------------------------------------------------------------------------------


scalarElem :: Ast.Scalar -> Elem ty
scalarElem (Ast.String  _) = String
scalarElem (Ast.Number  _) = Number
scalarElem (Ast.Bool _)    = Boolean
scalarElem Ast.Null        = Null

objectSubsetOf
    :: (Eq k, Hashable k)
    => (k -> ty) -> (ty -> ty -> K.Ternary) -> Object k ty -> Object k ty
    -> K.Ternary
objectSubsetOf keyTy sub l r =
    -- 1. All static keys in `r` must be present in the static keys of `l`.
    (K.fromBool $ Prelude.null $ objStatic r `HMS.difference` objStatic l) K.&&
    -- 2. All static keys in `l` must be either be subsets of static keys in
    -- `r`, or be a subset of the dynamic part of `r`.
    (K.all
        (\(k, lv) -> case HMS.lookup k (objStatic r) of
            Just rv -> lv `sub` rv
            Nothing -> case objDynamic r of
                Nothing         -> K.False
                Just (rdk, rdv) -> keyTy k `sub` rdk K.&& lv `sub` rdv)
        (HMS.toList (objStatic l))) K.&&
    -- 3. If `l` has a dynamic part, it must be a subset of `r`s dynamic part.
    -- If it does not have a dynamic part, `r` should not have one either.
    (case (objDynamic l, objDynamic r) of
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
    -- objectSubsetOf (const number) subsetOf x y
    subsetOf x y
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
unions = foldl' union empty

(∪) :: Type -> Type -> Type
(∪) = union


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

empty :: Type
empty = Union HS.empty

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

object :: Object Ast.Scalar Type -> Type
object = review singleton . Object

objectOf :: Type -> Type -> Type
objectOf k v = object $ Obj HMS.empty (Just (k, v))

arrayOf :: Type -> Type
-- arrayOf ty = review singleton $ Array $ Obj HMS.empty (Just (any, ty))
arrayOf = review singleton . Array

setOf :: Type -> Type
setOf = review singleton . Set

collectionOf :: Type -> Type
collectionOf x = arrayOf x ∪ setOf x ∪ objectOf any x
