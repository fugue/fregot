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
    , dataPackageNameFromText
    , dataPackageNameFromString

    , Var, unVar, mkVar
    , varToString, varToText
    , varFromText

    , Nested (..)
    , nestedToString

    , Key (..)
    , qualifiedVarFromKey
    , packageNameFromKey

    , UnqualifiedVar
    , QualifiedVar

    , Name (..), _LocalName, _QualifiedName
    , mkQualifiedName
    , nameToText
    , nameFromText

    , InstVar (..)

    , packageNameFromFilePath
    ) where

import           Control.Lens          (preview, review, (^.), (^?))
import           Control.Lens.Iso      (Iso', iso)
import           Control.Lens.Prism    (Prism', prism')
import           Control.Lens.TH       (makePrisms)
import           Control.Monad         (guard)
import qualified Data.Aeson            as Aeson
import           Data.Binary           (Binary)
import qualified Data.Binary           as Binary
import           Data.Hashable         (Hashable (..))
import qualified Data.List             as L
import           Data.String           (IsString (..))
import qualified Data.Text.Extended    as T
import           Data.Unique           (HasUnique (..), Unique, Uniquely (..))
import qualified Data.Unique           as Unique
import qualified Data.Vector           as V
import qualified Fregot.Lexer.Internal as Lexer
import qualified Fregot.PrettyPrint    as PP
import           GHC.Generics          (Generic)
import           System.FilePath       (dropTrailingPathSeparator, isRelative,
                                        joinPath, splitExtension, splitPath,
                                        (<.>))
import           System.IO.Unsafe      (unsafePerformIO)

data PackageName = PackageName {-# UNPACK #-} !Unique ![T.Text]
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

instance PP.Pretty PP.Sem PackageName where
    pretty =
        mconcat .  L.intersperse (PP.punctuation ".") .
        map (PP.namespace . PP.pretty) . unPackageName

instance Binary PackageName where
    get = mkPackageName <$> Binary.get
    put = Binary.put . unPackageName

-- TODO(jaspervdj): We can probably remove this instance now that we have scope
-- checking.
instance Semigroup PackageName where
    PackageName _ x <> PackageName _ y = mkPackageName (x <> y)

instance Monoid PackageName where
    mempty = mkPackageName mempty

unPackageName :: PackageName -> [T.Text]
unPackageName (PackageName _ t) = t

mkPackageName :: [T.Text] -> PackageName
mkPackageName ps =
    Unique.getStableUnique packageNameUniqueGen ps $ \u -> PackageName u ps

packageNameFromPieces :: [T.Text] -> Maybe PackageName
packageNameFromPieces []     = Nothing
packageNameFromPieces pieces = do
    guard $ all Lexer.varText pieces
    return $! mkPackageName pieces

packageNameUniqueGen :: Unique.StableUniqueGen [T.Text]
packageNameUniqueGen = unsafePerformIO Unique.newStableUniqueGen
{-# NOINLINE packageNameUniqueGen #-}

packageNameFromString :: Prism' String PackageName
packageNameFromString = T.fromString . packageNameFromText

packageNameFromText :: Prism' T.Text PackageName
packageNameFromText = prism'
    (T.intercalate "." . unPackageName)
    (packageNameFromPieces . T.split (== '.'))

-- | This prism understands both package names with and without a "data."
-- prefix.  The output needs to carry a boolean to indicate that in order to
-- satisfy the laws; but it is often thrown away.
dataPackageNameFromText :: Prism' T.Text (Bool, PackageName)
dataPackageNameFromText = prism' from to
  where
    from (True, pkg)  = "data." <> review packageNameFromText pkg
    from (False, pkg) = review packageNameFromText pkg
    to txt = case T.stripPrefix "data." txt of
        Nothing   -> (,) False <$> preview packageNameFromText txt
        Just txt' -> (,) True  <$> preview packageNameFromText txt'

dataPackageNameFromString :: Prism' String (Bool, PackageName)
dataPackageNameFromString = iso T.pack T.unpack . dataPackageNameFromText

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
    guard $ Lexer.varText txt
    return $ mkVar txt

-- | This type exists solely for pretty-printing.
newtype Nested a = Nested {unNested :: [a]}

nestedToString :: (a -> String) -> Nested a -> String
nestedToString f = L.intercalate "." . map f . unNested

instance PP.Pretty PP.Sem a => PP.Pretty PP.Sem (Nested a) where
    pretty = mconcat .
        L.intersperse (PP.punctuation ".") . map PP.pretty . unNested

newtype Key = Key {unKey :: V.Vector Var}
    deriving (Eq, Hashable, Monoid, Ord, Semigroup, Show)

instance PP.Pretty PP.Sem Key where
    pretty key = case key ^? qualifiedVarFromKey of
        Nothing         -> PP.pretty (key ^. packageNameFromKey)
        Just (pkg, var) -> PP.pretty pkg <> PP.punctuation "." <> PP.pretty var

instance IsString Key where
    fromString = Key . V.fromList . map mkVar . T.split (== '.') . T.pack

-- TODO(jaspervdj): This is quite inefficient and we're calling this a lot, so
-- we should look into this.
qualifiedVarFromKey :: Prism' Key QualifiedVar
qualifiedVarFromKey = prism'
    (\(pkg, v) -> Key . V.fromList $ map mkVar (unPackageName pkg) ++ [v])
    (\(Key k) -> if V.null k
        then Nothing
        else Just (mkPackageName . map unVar . V.toList $ V.init k, V.last k))

packageNameFromKey :: Iso' Key PackageName
packageNameFromKey = iso
    (\(Key vs) -> mkPackageName . map unVar $ V.toList vs)
    (\pkg      -> Key . V.fromList . map mkVar $ unPackageName pkg)

-- | Sometimes we want to be really clear in the source code that a variable
-- cannot be qualified in a specific position.
type UnqualifiedVar = Var

type QualifiedVar = (PackageName, Var)

data Name
    -- Variables inside rules
    = LocalName !Var
    -- Rules, functions
    | QualifiedName !Key
    -- Global names, used `data`, `input`, and for builtin functions such as
    -- `all`, `concat`...
    | BuiltinName !Var
    -- `_`
    | WildcardName
    deriving (Eq, Generic, Ord, Show)

instance Hashable Name

instance PP.Pretty PP.Sem Name where
    pretty = \case
        LocalName     v -> PP.pretty v
        QualifiedName k -> PP.pretty k
        BuiltinName   v -> PP.keyword (PP.pretty v)
        WildcardName    -> PP.punctuation "_"

$(makePrisms ''Name)

instance Aeson.ToJSON Name where
    toJSON = Aeson.toJSON . nameToText

mkQualifiedName :: PackageName -> Var -> Name
mkQualifiedName pkg var = QualifiedName $ review qualifiedVarFromKey (pkg, var)

nameToText :: Name -> T.Text
nameToText = \case
    LocalName v -> review varFromText v
    QualifiedName (Key k) ->
        T.intercalate "." . map (review varFromText) $ V.toList k
    BuiltinName v -> review varFromText v
    WildcardName -> "_"

-- | We cannot define a lawful Prism' because we don't convert to BuiltinName.
nameFromText :: T.Text -> Maybe Name
nameFromText txt = case T.breakOnEnd "." txt of
    ("", "_") -> pure WildcardName
    ("", var) -> LocalName <$> var ^? varFromText
    (pkgt, vart) -> do
        pkg <- T.init pkgt ^? packageNameFromText
        var <- vart ^? varFromText
        pure $ QualifiedName $ review qualifiedVarFromKey (pkg, var)

-- | An instantiated variable.  These have a unique (within the evaluation
-- context) number identifying them.  The var is just there for debugging
-- purposes.
data InstVar = InstVar {-# UNPACK #-} !Unique.Unique {-# UNPACK #-} !Var
    deriving Eq via Unique.Uniquely InstVar
    deriving Ord via Unique.Uniquely InstVar
    deriving Hashable via Unique.Uniquely InstVar

instance Unique.HasUnique InstVar where
    getUnique (InstVar u _) = u

instance Show InstVar where
    show (InstVar n v) = T.unpack (unVar v) ++ "_" ++ show n

instance PP.Pretty a InstVar where
    pretty = PP.pretty . show

packageNameFromFilePath :: Prism' FilePath PackageName
packageNameFromFilePath = prism' to from
  where
    to   p  = joinPath (fmap T.unpack $ unPackageName p) <.> "rego"
    from fp = do
        guard $ isRelative fp
        let (pieces, ext) = splitExtension fp
        guard $ ext == ".rego"
        packageNameFromPieces $
            fmap (T.pack . dropTrailingPathSeparator) $
            splitPath pieces
