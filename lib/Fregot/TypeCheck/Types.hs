{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
module Fregot.TypeCheck.Types
    ( ObjectType (..), otStatic, otDynamic
    , Type (..)
    , mergeTypes
    , intersectType

    , RuleType (..), _CompleteRuleType, _GenSetRuleType, _GenObjectRuleType
    , _FunctionType
    , ruleTypeToType

      -- Utilities
    , objectOf
    , collectionOf
    , scalarType
    ) where

import           Control.Lens        ((&), (.~), (^.))
import           Control.Lens.TH     (makeLenses, makePrisms)
import           Control.Monad       (guard)
import qualified Data.HashMap.Strict as HMS
import           Data.Maybe          (isNothing, maybeToList)
import qualified Fregot.Prepare.Ast  as Ast
import           Fregot.PrettyPrint  ((<+>))
import qualified Fregot.PrettyPrint  as PP

data ObjectType ty = ObjectType
    { _otStatic  :: HMS.HashMap Ast.Scalar ty
    , _otDynamic :: Maybe (ty, ty)
    } deriving (Eq, Functor, Show)

$(makeLenses ''ObjectType)

data Type
    = Any
    | Or Type Type
    | Array Type
    | Set Type
    | Object (ObjectType Type)
    | Number
    | String
    | Boolean
    | Null
    -- | 'Empty' should not actually appear in programs but is useful to have as
    -- identity element for 'Or'.
    | Empty
    deriving (Eq, Show)

instance PP.Pretty PP.Sem ty => PP.Pretty PP.Sem (ObjectType ty) where
    pretty ObjectType {..} = PP.object $
        [(PP.pretty' k, v) | (k, v) <- HMS.toList _otStatic] ++
        [ (PP.pretty otDynamicKey, otDynamicValue)
        | (otDynamicKey, otDynamicValue) <- maybeToList $ _otDynamic
        ]

instance PP.Pretty PP.Sem Type where
    pretty = \case
        Any           -> PP.keyword "any"
        Or x y        -> PP.pretty x <+> PP.punctuation "|" <+> PP.pretty y
        Array x       -> ho "array" (PP.pretty x)
        Set x         -> ho "set" (PP.pretty x)
        Object ot     -> PP.pretty ot
        Number        -> PP.keyword "number"
        String        -> PP.keyword "string"
        Boolean       -> PP.keyword "boolean"
        Null          -> PP.keyword "null"
        Empty         -> PP.keyword "empty"
      where
        ho t a = PP.keyword t <> PP.punctuation "<" <> a <> PP.punctuation ">"

data Reify
    = RAny
    | RUnion
        { ruArray   :: Maybe Reify
        , ruSet     :: Maybe Reify
        , ruObject  :: Maybe (ObjectType Reify)
        , ruNumber  :: Bool
        , ruString  :: Bool
        , ruBoolean :: Bool
        , ruNull    :: Bool
        }
    deriving (Show)

instance Semigroup ty => Semigroup (ObjectType ty) where
    l <> r = mempty
        & otStatic  .~ HMS.unionWith (<>) (l ^. otStatic) (r ^. otStatic)
        & otDynamic .~ (l ^. otDynamic <> r ^. otDynamic)

instance Semigroup ty => Monoid (ObjectType ty) where
    mempty = ObjectType HMS.empty Nothing

instance Semigroup Reify where
    RAny          <> _             = RAny
    _             <> RAny          = RAny
    l@(RUnion {}) <> r@(RUnion {}) = RUnion
        { ruArray   = ruArray   l <> ruArray   r
        , ruSet     = ruSet     l <> ruSet     r
        , ruObject  = ruObject  l <> ruObject  r
        , ruNumber  = ruNumber  l || ruNumber  r
        , ruString  = ruString  l || ruString  r
        , ruBoolean = ruBoolean l || ruBoolean r
        , ruNull    = ruNull    l || ruNull    r
        }

instance Monoid Reify where
    mempty = RUnion Nothing Nothing Nothing False False False False

reify :: Type -> Reify
reify Any        = RAny
reify (Or x y)   = reify x <> reify y
reify (Array x)  = mempty {ruArray   = Just (reify x)}
reify (Set x)    = mempty {ruSet     = Just (reify x)}
reify (Object o) = mempty {ruObject  = Just (fmap reify o)}
reify Number     = mempty {ruNumber  = True}
reify String     = mempty {ruString  = True}
reify Boolean    = mempty {ruBoolean = True}
reify Null       = mempty {ruNull    = True}
reify Empty      = mempty

abstract :: Reify -> Type
abstract RAny        = Any
abstract RUnion {..} =
    let list =
            [Array x  | x <- maybeToList $ abstract <$> ruArray] ++
            [Set x    | x <- maybeToList $ abstract <$> ruSet] ++
            [Object o | o <- maybeToList $ fmap abstract <$> ruObject] ++
            [Number   | ruNumber]  ++
            [String   | ruString]  ++
            [Boolean  | ruBoolean] ++
            [Null     | ruNull] in
    case list of
        []       -> Empty
        (x : xs) -> foldr Or x xs

empty :: Reify -> Bool
empty RAny        = False
empty RUnion {..} =
    isNothing ruArray  &&
    isNothing ruSet    &&
    isNothing ruObject &&
    not ruNumber       &&
    not ruString       &&
    not ruBoolean      &&
    not ruNull

emptyObject :: ObjectType Reify -> Bool
emptyObject obj = HMS.null (obj ^. otStatic) && isNothing (obj ^. otDynamic)

intersection :: Reify -> Reify -> Reify
intersection RAny          y             = y
intersection x             RAny          = x
intersection l@(RUnion {}) r@(RUnion {}) = RUnion
    { ruArray   = do
        array <- intersection <$> ruArray  l <*> ruArray  r
        guard $ not $ empty array
        pure array
    , ruSet     = do
        set <- intersection <$> ruSet l <*> ruSet r
        guard $ not $ empty set
        pure set
    , ruObject  = do
        obj <- intersectObjects <$> ruObject l <*> ruObject r
        guard $ not $ emptyObject obj
        pure obj
    , ruNumber  = ruNumber  l && ruNumber  r
    , ruString  = ruString  l && ruString  r
    , ruBoolean = ruBoolean l && ruBoolean r
    , ruNull    = ruNull    l && ruNull    r
    }

intersectObjects :: ObjectType Reify -> ObjectType Reify -> ObjectType Reify
intersectObjects l r = ObjectType
    { _otStatic = HMS.filter (not . empty) $
        (HMS.intersectionWith intersection (l ^. otStatic) (r ^. otStatic)) <.>
        ((l ^. otStatic) `diffStaticDynamic` (r ^. otDynamic)) <.>
        ((r ^. otStatic) `diffStaticDynamic` (l ^. otDynamic))
    , _otDynamic = do
        (lk, lv) <- l ^. otDynamic
        (rk, rv) <- r ^. otDynamic
        let k = intersection lk rk
            v = intersection lv rv
        guard $ not $ empty k
        guard $ not $ empty v
        pure (k, v)
    }
  where
    (<.>) = HMS.unionWith (<>)

    diffStaticDynamic _      Nothing             = HMS.empty
    diffStaticDynamic static (Just (dynk, dynv)) = HMS.mapMaybeWithKey
        (\statk statv -> do
            guard $ containsScalarType statk dynk
            pure $ intersection statv dynv)
        static

    containsScalarType :: Ast.Scalar -> Reify -> Bool
    containsScalarType (Ast.String _) = ruString
    containsScalarType (Ast.Number _) = ruNumber
    containsScalarType (Ast.Bool _)   = ruBoolean
    containsScalarType Ast.Null       = ruNull


mergeTypes :: [Type] -> Type
mergeTypes = abstract . foldMap reify

intersectType :: Type -> Type -> Type
intersectType x y = abstract $ intersection (reify x) (reify y)

data RuleType
    = CompleteRuleType Type
    | GenSetRuleType Type
    | GenObjectRuleType (ObjectType Type)
    | FunctionType  Int  -- TODO(jaspervdj)

instance PP.Pretty PP.Sem RuleType where
    pretty = PP.pretty . ruleTypeToType

-- | Converts a rule type to a normal type.
ruleTypeToType :: RuleType -> Type
ruleTypeToType (CompleteRuleType ty)   = ty
ruleTypeToType (GenSetRuleType ty)     = Set ty
ruleTypeToType (GenObjectRuleType oty) = Object oty
ruleTypeToType (FunctionType _)        = error "ruleTypeToType (FunctionType _)"

$(makePrisms ''RuleType)

objectOf :: Type -> Type -> Type
objectOf k v = Object (ObjectType HMS.empty (Just (k, v)))

collectionOf :: Type -> Type
collectionOf x = Array x `Or` Set x `Or` objectOf Any x

scalarType :: Ast.Scalar -> Type
scalarType = \case
    Ast.String _ -> String
    Ast.Number _ -> Number
    Ast.Bool   _ -> Boolean
    Ast.Null     -> Null
