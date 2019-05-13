-- | Variable- and name-like things.
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Names
    ( PackageName (..)
    , packageNameFromString, packageNameFromText
    , dataPackageNameFromString

    , Var, unVar, mkVar
    , varToString, varToText
    , varFromText
    , qualifiedVarFromText

    , NestedVar (..)
    , nestedVarToString
    ) where

import           Control.Lens       (review, (^?))
import           Control.Lens.Prism (Prism', prism')
import           Control.Monad      (guard)
import           Data.Binary        (Binary)
import qualified Data.Binary        as Binary
import           Data.Hashable      (Hashable (..))
import qualified Data.List          as L
import           Data.String        (IsString (..))
import qualified Data.Text.Extended as T
import           Data.Unique        (HasUnique (..), Unique, Uniquely (..))
import qualified Data.Unique        as Unique
import qualified Fregot.PrettyPrint as PP
import           System.IO.Unsafe   (unsafePerformIO)

newtype PackageName = PackageName {unPackageName :: [T.Text]}
    deriving (Binary, Eq, Hashable, Monoid, Ord, Semigroup, Show)

instance IsString PackageName where
    fromString = PackageName . T.split (== '.') . T.pack

instance PP.Pretty a PackageName where
    pretty = PP.pretty . review packageNameFromString

packageNameFromString :: Prism' String PackageName
packageNameFromString = T.fromString . packageNameFromText

packageNameFromText :: Prism' T.Text PackageName
packageNameFromText = prism'
    (T.intercalate "." . unPackageName)
    (\txt -> do
        let parts = T.split (== '.') txt
        guard $ not $ any T.null parts
        return $ PackageName parts)

-- | Like 'packageNameFromString', but with "data." prepended.
dataPackageNameFromString :: Prism' String PackageName
dataPackageNameFromString = prependData . packageNameFromString
  where
    prependData :: Prism' String String
    prependData = prism' ("data." ++) (L.stripPrefix "data.")

data Var = Var {-# UNPACK #-} !Unique {-# UNPACK #-} !T.Text
    deriving Eq via (Uniquely Var)
    deriving Hashable via (Uniquely Var)
    deriving Ord via (Uniquely Var)

instance Show Var where
    show = show . unVar

instance HasUnique Var where
    getUnique (Var u _) = u
    {-# INLINE getUnique #-}

instance IsString Var where
    fromString = mkVar . fromString

instance Binary Var where
    get = mkVar <$> Binary.get
    put = Binary.put . unVar

instance PP.Pretty a Var where
    pretty = PP.pretty . unVar

unVar :: Var -> T.Text
unVar (Var _ t) = t

mkVar :: T.Text -> Var
mkVar t = Var (Unique.getStableUnique varUniqueGen t) t

varUniqueGen :: Unique.StableUniqueGen T.Text
varUniqueGen = unsafePerformIO Unique.newStableUniqueGen
{-# NOINLINE varUniqueGen #-}

varToString :: Var -> String
varToString = T.unpack . varToText

varToText :: Var -> T.Text
varToText = unVar

varFromText :: Prism' T.Text Var
varFromText = prism' varToText $ \txt -> do
    guard $ T.all allowedChar txt
    return $ mkVar txt
  where
    allowedChar c =
        (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        (c >= '0' && c <= '9') ||
        c == '_'

qualifiedVarFromText :: Prism' T.Text (Maybe PackageName, Var)
qualifiedVarFromText = prism'
    (\(mbPkgName, var) -> case mbPkgName of
        Nothing      -> review varFromText var
        Just pkgName ->
            review packageNameFromText pkgName <> "." <> review varFromText var)
    (\txt -> case T.breakOnEnd "." txt of
        ("", var) -> ((,) Nothing) <$> var ^? varFromText
        (pkgName, var) -> (,)
            <$> (Just <$> T.init pkgName ^? packageNameFromText)
            <*> var ^? varFromText)

-- | This type exists solely for pretty-printing.
newtype NestedVar = NestedVar {unNestedVar :: [Var]}

nestedVarToString :: NestedVar -> String
nestedVarToString = L.intercalate "." . map varToString . unNestedVar

instance PP.Pretty PP.Sem NestedVar where
    pretty = mconcat .
        L.intersperse (PP.punctuation ".") . map PP.pretty . unNestedVar
