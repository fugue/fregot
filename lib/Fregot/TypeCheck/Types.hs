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

    , RuleType (..), _CompleteRuleType, _GenSetRuleType, _GenObjectRuleType
    , _FunctionType
    , ruleTypeToType

      -- Utilities
    , objectOf
    , collectionOf
    ) where

import           Control.Lens        ((&), (.~), (^.))
import           Control.Lens.TH     (makeLenses, makePrisms)
import qualified Data.HashMap.Strict as HMS
import           Data.Maybe          (maybeToList)
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
    deriving (Eq)

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

data MergeType
    = MergeAny
    | MergeOr
        { moArray   :: Maybe MergeType
        , moSet     :: Maybe MergeType
        , moObject  :: Maybe (ObjectType MergeType)
        , moNumber  :: Bool
        , moString  :: Bool
        , moBoolean :: Bool
        , moNull    :: Bool
        }
    deriving (Show)

instance Semigroup ty => Semigroup (ObjectType ty) where
    l <> r = mempty
        & otStatic  .~ HMS.unionWith (<>) (l ^. otStatic) (r ^. otStatic)
        & otDynamic .~ (l ^. otDynamic <> r ^. otDynamic)

instance Semigroup ty => Monoid (ObjectType ty) where
    mempty = ObjectType HMS.empty Nothing

instance Semigroup MergeType where
    MergeAny       <> _              = MergeAny
    _              <> MergeAny       = MergeAny
    l@(MergeOr {}) <> r@(MergeOr {}) = MergeOr
        { moArray   = moArray   l <> moArray   r
        , moSet     = moSet     l <> moSet     r
        , moObject  = moObject  l <> moObject  r
        , moNumber  = moNumber  l || moNumber  r
        , moString  = moString  l || moString  r
        , moBoolean = moBoolean l || moBoolean r
        , moNull    = moNull    l || moNull    r
        }

instance Monoid MergeType where
    mempty = MergeOr Nothing Nothing Nothing False False False False

toMergeType :: Type -> MergeType
toMergeType Any        = MergeAny
toMergeType (Or x y)   = toMergeType x <> toMergeType y
toMergeType (Array x)  = mempty {moArray   = Just (toMergeType x)}
toMergeType (Set x)    = mempty {moSet     = Just (toMergeType x)}
toMergeType (Object o) = mempty {moObject  = Just (fmap toMergeType o)}
toMergeType Number     = mempty {moNumber  = True}
toMergeType String     = mempty {moString  = True}
toMergeType Boolean    = mempty {moBoolean = True}
toMergeType Null       = mempty {moNull    = True}
toMergeType Empty      = mempty

fromMergeType :: MergeType -> Type
fromMergeType MergeAny     = Any
fromMergeType MergeOr {..} =
    let list =
            [Array x  | x <- maybeToList $ fromMergeType <$> moArray] ++
            [Set x    | x <- maybeToList $ fromMergeType <$> moSet] ++
            [Object o | o <- maybeToList $ fmap fromMergeType <$> moObject] ++
            [Number   | moNumber]  ++
            [String   | moString]  ++
            [Boolean  | moBoolean] ++
            [Null     | moNull] in
    case list of
        []       -> Empty
        (x : xs) -> foldr Or x xs

mergeTypes :: [Type] -> Type
mergeTypes = fromMergeType . foldMap toMergeType

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
