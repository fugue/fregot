{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Fregot.Eval.Mu
    ( MuF (..)
    , Mu (..)
    , describeMu
    , describeMuF

    , muValue
    , muValueF
    , muTrue

    , muToString
    , muToNumber
    ) where

import           Control.Lens       (review)
import           Data.Hashable      (Hashable)
import qualified Data.Text          as T
import           Fregot.Eval.Number (Number)
import           Fregot.Eval.Value
import           Fregot.Names
import           Fregot.PrettyPrint ((<+>))
import qualified Fregot.PrettyPrint as PP
import           GHC.Generics       (Generic)

data MuF a
    = RecM (ValueF a)
    | GroundedM Value
    | WildcardM
    | FreeM {-# UNPACK #-} !InstVar
    -- | Packages are definitely not first-class values but we can pretend that
    -- they are.
    --
    -- TODO(jaspervdj): Replace with Key or Tree.
    | PackageM !PackageName
    deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance Hashable a => Hashable (MuF a)

instance PP.Pretty PP.Sem a => PP.Pretty PP.Sem (MuF a) where
    pretty (RecM      v) = PP.pretty v
    pretty (GroundedM v) = PP.pretty v
    pretty (FreeM     v) = PP.pretty v
    pretty WildcardM     = "_"
    pretty (PackageM  p) = PP.keyword "package" <+> PP.pretty p

newtype Mu = Mu {unMu :: MuF Mu}
    deriving (Eq, Generic, Hashable, Ord, PP.Pretty PP.Sem, Show)

describeMu :: Mu -> String
describeMu = describeMuF . unMu

describeMuF :: MuF a -> String
describeMuF = \case
    RecM      v -> describeValueF v
    GroundedM v -> describeValue v
    FreeM     v -> "free variable (" ++ show v ++ ")"
    WildcardM   -> "wildcard"
    PackageM  p -> "package " ++ review packageNameFromString p

muValue :: Value -> Mu
muValue = Mu . GroundedM

muValueF :: ValueF Value -> Mu
muValueF = muValue . Value

muTrue :: Mu
muTrue = muValue true

muToString :: Mu -> Maybe T.Text
muToString (Mu (GroundedM (Value (StringV t)))) = Just t
muToString (Mu (RecM (StringV t)))              = Just t
muToString _                                    = Nothing

muToNumber :: Mu -> Maybe Number
muToNumber (Mu (GroundedM (Value (NumberV n)))) = Just n
muToNumber (Mu (RecM (NumberV n)))              = Just n
muToNumber _                                    = Nothing
