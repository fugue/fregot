{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
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

import           Control.Lens              (preview, re)
import           Data.Maybe                (fromMaybe)
import qualified Data.Text                 as T
import           Fregot.Eval.Number        (Number)
import           Fregot.Eval.Value
import           Fregot.Names
import           Fregot.Prepare.Ast        (Rule)
import           Fregot.PrettyPrint        ((<+>))
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)
import qualified Fregot.Tree               as Tree
import           Fregot.Types.Rule         (RuleType)
import           GHC.Generics              (Generic)

data MuF e a
    = RecM (ValueF a)
    | GroundedM Value
    | WildcardM
    | FreeM {-# UNPACK #-} !InstVar
    -- | It is appropriate to view 'TreeM' as a thunk/suspension; since it
    -- contains rules that will be evaluated later.  As such, we need to make
    -- sure they are evaluated in the original environment, passed in as 'e'.
    --
    -- Having a shallower embedding where we have a tree of lazily (in Haskell)
    -- evaluated rules would prevent us from carrying 'e' here.
    | TreeM !e !Key !(Tree.Tree (Rule RuleType SourceSpan))
    deriving (Foldable, Functor, Generic, Show, Traversable)

instance PP.Pretty PP.Sem a => PP.Pretty PP.Sem (MuF e a) where
    pretty (RecM      v) = PP.pretty v
    pretty (GroundedM v) = PP.pretty v
    pretty (FreeM     v) = PP.pretty v
    pretty WildcardM     = "_"
    pretty (TreeM _ k _) = PP.keyword "package" <+> PP.pretty k

newtype Mu e = Mu {unMu :: MuF e (Mu e)}
    deriving (Generic, PP.Pretty PP.Sem, Show)

describeMu :: Mu e -> String
describeMu = describeMuF . unMu

describeMuF :: MuF e a -> String
describeMuF = \case
    RecM      v -> describeValueF v
    GroundedM v -> describeValue v
    FreeM     v -> "free variable (" ++ show v ++ ")"
    WildcardM   -> "wildcard"
    TreeM _ k _ -> "package " ++ fromMaybe "<root>"
        (preview (packageNameFromKey . re packageNameFromString) k)

muValue :: Value -> Mu e
muValue = Mu . GroundedM

muValueF :: ValueF Value -> Mu e
muValueF = muValue . Value

muTrue :: Mu e
muTrue = muValue true

muToString :: Mu e -> Maybe T.Text
muToString (Mu (GroundedM (Value (StringV t)))) = Just t
muToString (Mu (RecM (StringV t)))              = Just t
muToString _                                    = Nothing

muToNumber :: Mu e -> Maybe Number
muToNumber (Mu (GroundedM (Value (NumberV n)))) = Just n
muToNumber (Mu (RecM (NumberV n)))              = Just n
muToNumber _                                    = Nothing
