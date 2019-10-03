{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
module Fregot.TypeCheck.Types
    ( Type (..)
    , RuleType (..), _CompleteRuleType, _GenSetRuleType, _GenObjectRuleType
    , _FunctionType

    , mergeTypes
    ) where

import           Control.Lens.TH    (makePrisms)
import           Fregot.PrettyPrint ((<+>))
import qualified Fregot.PrettyPrint as PP

data Type
    = Any
    | Or Type Type
    | Number
    | String
    | Boolean
    | Null
    -- | 'Empty' should not actually appear in programs but is useful to have as
    -- identity element for 'Or'.
    | Empty
    deriving (Eq)

instance PP.Pretty PP.Sem Type where
    pretty Any      = PP.keyword "any"
    pretty (Or x y) = PP.pretty x <+> PP.punctuation "|" <+> PP.pretty y
    pretty Number   = PP.keyword "number"
    pretty String   = PP.keyword "string"
    pretty Boolean  = PP.keyword "boolean"
    pretty Null     = PP.keyword "null"
    pretty Empty    = PP.keyword "empty"

data RuleType
    = CompleteRuleType Type
    | GenSetRuleType Type
    | GenObjectRuleType Type Type
    | FunctionType  Int  -- TODO(jaspervdj)

instance PP.Pretty PP.Sem RuleType where
    pretty = \case
        CompleteRuleType ty -> "rule" <+> PP.punctuation "=" <+> PP.pretty ty
        GenSetRuleType ty -> "rule" <+> PP.punctuation "[" <+>
            PP.pretty ty <+> PP.punctuation "]"
        GenObjectRuleType idx ty -> "rule" <+> PP.punctuation "[" <+>
            PP.pretty idx <+> PP.punctuation "]" <+> PP.punctuation "=" <+>
            PP.pretty ty
        FunctionType arity -> "rule" <+> PP.punctuation "(" <+>
            PP.commaSep (replicate arity "_") <+> PP.punctuation ")" <+>
            PP.punctuation "=" <+> "_"

$(makePrisms ''RuleType)

data MergeType
    = MergeAny
    | MergeOr
        { moNumber  :: Bool
        , moString  :: Bool
        , moBoolean :: Bool
        , moNull    :: Bool
        }
    deriving (Show)

instance Semigroup MergeType where
    MergeAny       <> _              = MergeAny
    _              <> MergeAny       = MergeAny
    l@(MergeOr {}) <> r@(MergeOr {}) = MergeOr
        { moNumber  = moNumber  l || moNumber  r
        , moString  = moString  l || moString  r
        , moBoolean = moBoolean l || moBoolean r
        , moNull    = moNull    l || moNull    r
        }

instance Monoid MergeType where
    mempty = MergeOr False False False False

toMergeType :: Type -> MergeType
toMergeType Any      = MergeAny
toMergeType (Or x y) = toMergeType x <> toMergeType y
toMergeType Number   = mempty {moNumber  = True}
toMergeType String   = mempty {moString  = True}
toMergeType Boolean  = mempty {moBoolean = True}
toMergeType Null     = mempty {moNull    = True}
toMergeType Empty    = mempty

fromMergeType :: MergeType -> Type
fromMergeType MergeAny     = Any
fromMergeType MergeOr {..} =
    let list =
            [Number  | moNumber]  ++
            [String  | moString]  ++
            [Boolean | moBoolean] ++
            [Null    | moNull] in
    case list of
        []       -> Empty
        (x : xs) -> foldr Or x xs

mergeTypes :: [Type] -> Type
mergeTypes = fromMergeType . foldMap toMergeType
