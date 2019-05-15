-- | Variable- and name-like things.
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Names
    ( PackageName (..), unPackageName, mkPackageName
    , packageNameFromString, packageNameFromText
    , dataPackageNameFromString

    , Var, unVar, mkVar
    , varToString, varToText
    , varFromText

    , Nested (..)
    , nestedToString

    , UnqualifiedVar

    , Name (..), _LocalName, _QualifiedName
    , nameToText
    , nameFromText

    , Imports
    ) where

import           Control.Lens        (review, (^?))
import           Control.Lens.Prism  (Prism', prism')
import           Control.Lens.TH     (makePrisms)
import           Control.Monad       (guard)
import           Data.Binary         (Binary)
import qualified Data.Binary         as Binary
import           Data.Hashable       (Hashable (..))
import qualified Data.HashMap.Strict as HMS
import qualified Data.List           as L
import           Data.String         (IsString (..))
import qualified Data.Text.Extended  as T
import           Data.Unique         (HasUnique (..), Unique, Uniquely (..))
import qualified Data.Unique         as Unique
import qualified Fregot.PrettyPrint  as PP
import           GHC.Generics        (Generic)
import           System.IO.Unsafe    (unsafePerformIO)

data PackageName = PackageName {-# UNPACK #-} !Unique [T.Text]
    deriving Eq via (Uniquely PackageName)
    deriving Hashable via (Uniquely PackageName)
    deriving Ord via (Uniquely PackageName)

instance HasUnique PackageName where
    getUnique (PackageName u _) = u
    {-# INLINE getUnique #-}

instance IsString PackageName where
    fromString = mkPackageName . T.split (== '.') . T.pack

instance Show PackageName where
    show = review packageNameFromString

instance PP.Pretty a PackageName where
    pretty = PP.pretty . review packageNameFromString

instance Binary PackageName where
    get = mkPackageName <$> Binary.get
    put = Binary.put . unPackageName

-- TODO(jaspervdj): We can probably remove this instance now that we have scope
-- checking.
instance Semigroup PackageName where
    x <> y = mkPackageName (unPackageName x <> unPackageName y)

instance Monoid PackageName where
    mempty = mkPackageName mempty

unPackageName :: PackageName -> [T.Text]
unPackageName (PackageName _ t) = t

mkPackageName :: [T.Text] -> PackageName
mkPackageName ps =
    Unique.getStableUnique packageNameUniqueGen ps $ \u -> PackageName u ps

packageNameUniqueGen :: Unique.StableUniqueGen [T.Text]
packageNameUniqueGen = unsafePerformIO Unique.newStableUniqueGen
{-# NOINLINE packageNameUniqueGen #-}

packageNameFromString :: Prism' String PackageName
packageNameFromString = T.fromString . packageNameFromText

packageNameFromText :: Prism' T.Text PackageName
packageNameFromText = prism'
    (T.intercalate "." . unPackageName)
    (\txt -> do
        let parts = T.split (== '.') txt
        guard $ not $ any T.null parts
        return $ mkPackageName parts)

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
mkVar t = Unique.getStableUnique varUniqueGen t $ \u -> Var u t

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

-- | This type exists solely for pretty-printing.
newtype Nested a = Nested {unNested :: [a]}

nestedToString :: (a -> String) -> Nested a -> String
nestedToString f = L.intercalate "." . map f . unNested

instance PP.Pretty PP.Sem a => PP.Pretty PP.Sem (Nested a) where
    pretty = mconcat .
        L.intersperse (PP.punctuation ".") . map PP.pretty . unNested

-- | Sometimes we want to be really clear in the source code that a variable
-- cannot be qualified in a specific position.
type UnqualifiedVar = Var

data Name
    -- Variables inside rules
    = LocalName !Var
    -- Rules, functions
    | QualifiedName !PackageName !Var
    -- Global names, used for builtin functions such ass all, concat...
    | BuiltinName !Var
    deriving (Eq, Generic, Ord, Show)

instance Hashable Name

instance PP.Pretty PP.Sem Name where
    pretty = \case
        LocalName       v -> PP.pretty v
        QualifiedName p v -> PP.pretty p <> PP.punctuation "." <> PP.pretty v
        BuiltinName     v -> PP.keyword (PP.pretty v)

$(makePrisms ''Name)

nameToText :: Name -> T.Text
nameToText = \case
    LocalName v -> review varFromText v
    QualifiedName p v ->
        review packageNameFromText p <> "." <> review varFromText v
    BuiltinName v -> review varFromText v

-- | We cannot define a lawful Prism' because we don't convert to BuiltinName.
nameFromText :: T.Text -> Maybe Name
nameFromText txt = case T.breakOnEnd "." txt of
    ("", var) -> LocalName <$> var ^? varFromText
    (pkgName, var) -> QualifiedName
        <$> T.init pkgName ^? packageNameFromText
        <*> var ^? varFromText

type Imports a = HMS.HashMap Var (a, PackageName)
