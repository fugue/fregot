{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Sugar
    ( PackageName (..)
    , packageNameToString, packageNameToText

    , Import (..), importAnn, importPackage, importAs
    , Module (..), modulePackage, moduleImports, modulePolicy

    , Var (..)
    , varToString, varToText

    , Rule (..), ruleHead, ruleBody
    , RuleHead (..), ruleAnn, ruleDefault, ruleName, ruleArgs, ruleIndex, ruleValue
    , RuleBody
    , Literal (..), literalNegation, literalExpr, literalWith

    , Expr (..), exprAnn
    , Term (..), termAnn
    , RefArg (..)
    , Scalar (..)
    , Object
    , ObjectKey (..)
    , BinOp (..)
    , With (..), withAnn, withWith, withAs
    ) where

import           Control.Lens       (Lens', lens, (^.))
import           Control.Lens.TH    (makeLenses)
import           Data.Hashable      (Hashable (..))
import qualified Data.List          as L
import           Data.Scientific    (Scientific)
import           Data.String        (IsString (..))
import qualified Data.Text          as T
import           Fregot.PrettyPrint ((<$$>), (<+>), (<+>?), (?<+>))
import qualified Fregot.PrettyPrint as PP

newtype PackageName = PackageName {unPackageName :: [T.Text]}
    deriving (Eq, Hashable, Monoid, Ord, Semigroup, Show)

instance IsString PackageName where
    fromString = PackageName . T.split (== '.') . T.pack

packageNameToString :: PackageName -> String
packageNameToString = T.unpack . packageNameToText

packageNameToText :: PackageName -> T.Text
packageNameToText = T.intercalate "." . unPackageName

data Import a = Import
    { _importAnn     :: !a
    , _importPackage :: !PackageName
    , _importAs      :: !(Maybe Var)
    } deriving (Show)

data Module a = Module
    { _modulePackage :: !PackageName
    , _moduleImports :: ![Import a]
    , _modulePolicy  :: ![Rule a]
    } deriving (Show)

newtype Var = Var {unVar :: T.Text}
    deriving (Hashable, Eq, Ord, Show)

instance IsString Var where
    fromString = Var . fromString

varToString :: Var -> String
varToString = T.unpack . varToText

varToText :: Var -> T.Text
varToText = unVar

data Rule a = Rule
    { _ruleHead :: !(RuleHead a)
    , _ruleBody :: !(RuleBody a)
    } deriving (Show)

data RuleHead a = RuleHead
    { _ruleAnn     :: !a
    , _ruleDefault :: !Bool
    , _ruleName    :: !Var
    , _ruleArgs    :: !(Maybe [Term a])
    , _ruleIndex   :: !(Maybe (Term a))
    , _ruleValue   :: !(Maybe (Term a))
    } deriving (Show)

type RuleBody a = [Literal a]

data Literal a = Literal
    { _literalNegation :: !Bool
    , _literalExpr     :: !(Expr a)
    , _literalWith     :: ![With a]
    } deriving (Show)

data Expr a
    = TermE   a (Term a)
    -- NOTE(jaspervdj): Perhaps assign should be part of 'Literal'.  It is
    -- missing from the grammar, so I'm not sure at this point.
    | BinOpE  a (Expr a) BinOp (Expr a)
    | ParensE a (Expr a)
    deriving (Show)

data Term a
    = RefT      a a Var [RefArg a]
    -- NOTE(jaspervdj): According to the grammar, a function call should be an
    -- expression, not a term.  Putting it here is more permissive though, so I
    -- hope that won't cause any trouble.
    | CallT     a [Var] [Term a]
    | VarT      a Var
    | ScalarT   a (Scalar a)

    | ArrayT    a [Expr a]
    | SetT      a [Expr a]
    | ObjectT   a (Object a)

    | ArrayCompT  a (Term a) (RuleBody a)
    | SetCompT    a (Term a) (RuleBody a)
    | ObjectCompT a (ObjectKey a) (Term a) (RuleBody a)
    deriving (Show)

data RefArg a
    = RefBrackArg !(Expr a)
    | RefDotArg !a !Var
    deriving (Show)

data Scalar a
    = String T.Text
    | Number Scientific
    | Bool   Bool
    | Null
    deriving (Show)

type Object a = [(ObjectKey a, Expr a)]

data ObjectKey a
    = ScalarK a (Scalar a)
    | VarK    a Var
    | RefK    a Var [RefArg a]
    deriving (Show)

data BinOp
    = UnifyO
    | AssignO
    | EqualO
    | NotEqualO
    | LessThanO
    | LessThanOrEqualO
    | GreaterThanO
    | GreaterThanOrEqualO
    | PlusO
    | MinusO
    | TimesO
    | DivideO
    deriving (Show)

data With a = With
    { _withAnn  :: !a
    , _withWith :: ![Var]
    , _withAs   :: !(Term a)
    } deriving (Show)

$(makeLenses ''Import)
$(makeLenses ''Module)
$(makeLenses ''Rule)
$(makeLenses ''RuleHead)
$(makeLenses ''Literal)
$(makeLenses ''With)

instance PP.Pretty a PackageName where
    pretty = PP.pretty . packageNameToString

instance PP.Pretty PP.Sem (Import a) where
    pretty imp =
        PP.keyword "import" <+> PP.pretty (imp ^. importPackage) <+>?
        fmap PP.pretty (imp ^. importAs)

instance PP.Pretty PP.Sem (Module a) where
    pretty pkg = PP.vcat2 $
        (PP.keyword "package" <+> PP.pretty (pkg ^. modulePackage)) :
        (case pkg ^. moduleImports of
            []   -> []
            imps -> [PP.vcat $ map PP.pretty imps]) ++
        map PP.pretty (pkg ^. modulePolicy)

instance PP.Pretty a Var where
    pretty = PP.pretty . unVar

instance PP.Pretty PP.Sem (Rule a) where
    pretty r =
        PP.pretty (r ^. ruleHead) <+>?
        (case r ^. ruleBody of
            [] -> Nothing
            bs -> Just $
                PP.punctuation "{" <$$>
                PP.ind (PP.vcat $ map (\b -> PP.pretty b) bs) <$$>
                PP.punctuation "}")

instance PP.Pretty PP.Sem (RuleHead a) where
    pretty r =
        (if r ^. ruleDefault then Just (PP.keyword "default") else Nothing) ?<+>
        PP.pretty (r ^. ruleName) <>
        (case r ^. ruleArgs of
            Nothing   -> mempty
            Just args ->
                PP.punctuation "(" <> PP.commaSep (map PP.pretty args) <>
                PP.punctuation ")") <>
        (case r ^. ruleIndex of
            Nothing  -> mempty
            Just idx ->
                PP.punctuation "[" <> PP.pretty idx <> PP.punctuation "]") <>
        (case r ^. ruleValue of
            Nothing  -> mempty
            Just val -> PP.space <>
                PP.punctuation "=" <+> PP.pretty val)

instance PP.Pretty PP.Sem (Literal a) where
    pretty lit =
        (if lit ^. literalNegation
            then Just (PP.keyword "not")
            else Nothing) ?<+>
        PP.pretty (lit ^. literalExpr) <+>?
        (case lit ^. literalWith of
            []    -> Nothing
            withs -> Just $ PP.hcat (L.intersperse " " (map PP.pretty withs)))

instance PP.Pretty PP.Sem (Expr a) where
    pretty (TermE _ t)      = PP.pretty t
    pretty (BinOpE _ x o y) = PP.pretty x <+> PP.pretty o <+> PP.pretty y
    pretty (ParensE _ e)    =
        PP.punctuation "(" <> PP.pretty e <> PP.punctuation ")"

instance PP.Pretty PP.Sem (Term a) where
    pretty (RefT _ _ v args) = PP.pretty v <> mconcat (map PP.pretty args)
    pretty (CallT _ vs as)  =
        mconcat (L.intersperse (PP.punctuation ".") (map PP.pretty vs)) <>
        PP.punctuation "(" <>
        PP.commaSep (map PP.pretty as) <>
        PP.punctuation ")"

    pretty (VarT _ v)        = PP.pretty v
    pretty (ScalarT _ s)     = PP.pretty s

    pretty (ArrayT _ a)      = PP.array a
    pretty (SetT _ s)        = PP.set s
    pretty (ObjectT _ o)     = PP.object o

    pretty (ArrayCompT _ x lits) =
        PP.punctuation "[" <> PP.pretty x <+> PP.punctuation "|" <+>
        prettyComprehensionBody lits <>
        PP.punctuation "]"
    pretty (SetCompT _ x lits) =
        PP.punctuation "{" <> PP.pretty x <+> PP.punctuation "|" <+>
        prettyComprehensionBody lits <>
        PP.punctuation "}"
    pretty (ObjectCompT _ k x lits) =
        PP.punctuation "{" <> PP.pretty k <> PP.punctuation ":" <+>
        PP.pretty x <+> PP.punctuation "|" <+>
        prettyComprehensionBody lits <>
        PP.punctuation "}"

prettyComprehensionBody :: RuleBody a -> PP.SemDoc
prettyComprehensionBody lits = mconcat $ L.intersperse
    (PP.punctuation ";" <> PP.space)
    (map PP.pretty lits)

instance PP.Pretty PP.Sem (RefArg a) where
    pretty (RefBrackArg a) =
        PP.punctuation "[" <> PP.pretty a <> PP.punctuation "]"
    pretty (RefDotArg _ k) =
        PP.punctuation "." <> PP.pretty k

instance PP.Pretty PP.Sem (Scalar a) where
    pretty (String s) = PP.literal $ PP.pretty $ show $ T.unpack s
    pretty (Number s) = PP.literal $ PP.pretty s
    pretty (Bool   b) = PP.literal $ if b then "true" else "false"
    pretty Null       = PP.literal "null"

instance PP.Pretty PP.Sem (ObjectKey a) where
    pretty (ScalarK _ s) = PP.pretty s
    pretty (VarK _ v)    = PP.pretty v
    pretty (RefK _ v a)  = PP.pretty v <> mconcat (map PP.pretty a)

instance PP.Pretty PP.Sem BinOp where
    pretty bo = PP.punctuation $ case bo of
        UnifyO              -> "="
        AssignO             -> ":="
        EqualO              -> "=="
        NotEqualO           -> "!="
        LessThanO           -> "<"
        LessThanOrEqualO    -> "<="
        GreaterThanO        -> ">"
        GreaterThanOrEqualO -> ">="
        PlusO               -> "+"
        MinusO              -> "-"
        TimesO              -> "*"
        DivideO             -> "/"

instance PP.Pretty PP.Sem (With a) where
    pretty with = PP.keyword "with" <+>
        (mconcat $ L.intersperse
            (PP.punctuation ".")
            (map PP.pretty (with ^. withWith))) <+>
        PP.keyword "as" <+> PP.pretty (with ^. withAs)

exprAnn :: Lens' (Expr a) a
exprAnn = lens getAnn setAnn
  where
    getAnn = \case
        TermE   a _     -> a
        BinOpE  a _ _ _ -> a
        ParensE a _     -> a

    setAnn e a = case e of
        TermE   _ t     -> TermE   a t
        BinOpE  _ x o y -> BinOpE  a x o y
        ParensE _ x     -> ParensE a x

termAnn :: Lens' (Term a) a
termAnn = lens getAnn setAnn
  where
    getAnn = \case
        RefT        a _ _ _ -> a
        CallT       a _ _   -> a
        VarT        a _     -> a
        ScalarT     a _     -> a
        ArrayT      a _     -> a
        SetT        a _     -> a
        ObjectT     a _     -> a
        ArrayCompT  a _ _   -> a
        SetCompT    a _ _   -> a
        ObjectCompT a _ _ _ -> a

    setAnn t a = case t of
        RefT        _ b v r -> RefT        a b v r
        CallT       _ f as  -> CallT       a f as
        VarT        _ v     -> VarT        a v
        ScalarT     _ s     -> ScalarT     a s
        ArrayT      _ l     -> ArrayT      a l
        SetT        _ s     -> SetT        a s
        ObjectT     _ o     -> ObjectT     a o
        ArrayCompT  _ x b   -> ArrayCompT  a x b
        SetCompT    _ x b   -> SetCompT    a x b
        ObjectCompT _ k x b -> ObjectCompT a k x b
