{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Sugar
    ( PackageName (..)
    , Import (..), importAnn, importPackage, importAs
    , Package (..), packageName, packageImports, packagePolicy

    , Var (..)
    , Rule (..), ruleHead, ruleBody
    , RuleHead (..), ruleName, ruleAnn, ruleIndex, ruleValue
    , Literal (..)
    , Expr (..)
    , Term (..)
    , RefArg (..)
    , Scalar (..)
    , Object
    , ObjectKey (..)
    , BinOp (..)

    , exprAnn
    ) where

import           Control.Lens       ((^.))
import           Control.Lens.TH    (makeLenses)
import           Data.Hashable      (Hashable (..))
import           Data.Scientific    (Scientific)
import qualified Data.Text          as T
import           Fregot.PrettyPrint ((<$$>), (<+>), (<+>?))
import qualified Fregot.PrettyPrint as PP

newtype PackageName = PackageName {unPackageName :: [T.Text]}
    deriving (Hashable, Eq, Ord, Show)

data Import a = Import
    { _importAnn     :: !a
    , _importPackage :: !PackageName
    , _importAs      :: !(Maybe Var)
    } deriving (Show)

data Package a = Package
    { _packageName    :: !PackageName
    , _packageImports :: ![Import a]
    , _packagePolicy  :: ![Rule a]
    } deriving (Show)

newtype Var = Var {unVar :: T.Text}
    deriving (Hashable, Eq, Ord, Show)

-- TODO:
-- * default
data Rule a = Rule
    { _ruleHead :: !(RuleHead a)
    , _ruleBody :: ![Literal a]
    } deriving (Show)

-- TODO
-- * args
data RuleHead a = RuleHead
    { _ruleName  :: !Var
    , _ruleAnn   :: !a
    , _ruleIndex :: !(Maybe (Term a))
    , _ruleValue :: !(Maybe (Term a))
    } deriving (Show)

-- TODO:
-- * with-modifier
data Literal a
    = ExprL    (Expr a)
    | NotExprL (Expr a)
    deriving (Show)

data Expr a
    = TermE a (Term a)
    | UnifyE a (Expr a) (Expr a)
    | BinOpE a (Expr a) BinOp (Expr a)
    | ParensE a (Expr a)
    deriving (Show)

data Term a
    = RefT      a a Var [RefArg a]
    | VarT      a Var
    | ScalarT   a (Scalar a)
    | ArrayT    a [Expr a]
    | ObjectT   a (Object a)
    deriving (Show)

data RefArg a
    = RefBrackArg  !(Term a)
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
    deriving (Show)

data BinOp
    = AssignO
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

$(makeLenses ''Import)
$(makeLenses ''Package)
$(makeLenses ''Rule)
$(makeLenses ''RuleHead)

instance PP.Pretty a PackageName where
    pretty = PP.pretty . T.intercalate "." . unPackageName

instance PP.Pretty PP.Sem (Import a) where
    pretty imp =
        PP.keyword "import" <+> PP.pretty (imp ^. importPackage) <+>?
        fmap PP.pretty (imp ^. importAs)

instance PP.Pretty PP.Sem (Package a) where
    pretty pkg = PP.vcat2 $
        (PP.keyword "package" <+> PP.pretty (pkg ^. packageName)) :
        (case pkg ^. packageImports of
            []   -> []
            imps -> [PP.vcat $ map PP.pretty imps]) ++
        map PP.pretty (pkg ^. packagePolicy)

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
        PP.pretty (r ^. ruleName) <>
        (case r ^. ruleIndex of
            Nothing  -> mempty
            Just idx ->
                PP.punctuation "[" <> PP.pretty idx <> PP.punctuation "]") <>
        (case r ^. ruleValue of
            Nothing  -> mempty
            Just val -> PP.space <>
                PP.punctuation "=" <+> PP.pretty val)

instance PP.Pretty PP.Sem (Literal a) where
    pretty (ExprL e)    = PP.pretty e
    pretty (NotExprL e) = PP.keyword "not" <+> PP.pretty e

instance PP.Pretty PP.Sem (Expr a) where
    pretty (TermE _ t)      = PP.pretty t
    pretty (UnifyE _ x y)   = PP.pretty x <+> PP.punctuation "=" <+> PP.pretty y
    pretty (BinOpE _ x o y) = PP.pretty x <+> PP.pretty o <+> PP.pretty y
    pretty (ParensE _ e)    =
        PP.punctuation "(" <> PP.pretty e <> PP.punctuation ")"

instance PP.Pretty PP.Sem (Term a) where
    pretty (RefT _ _ v args) = PP.pretty v <> mconcat (map PP.pretty args)
    pretty (VarT _ v)        = PP.pretty v
    pretty (ScalarT _ s)     = PP.pretty s
    pretty (ArrayT _ a)      = PP.array a
    pretty (ObjectT _ o)     = PP.object o

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

instance PP.Pretty PP.Sem BinOp where
    pretty bo = PP.punctuation $ case bo of
        AssignO             -> "="
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

exprAnn :: Expr a -> a
exprAnn = \case
    TermE a _      -> a
    UnifyE a _ _   -> a
    BinOpE a _ _ _ -> a
    ParensE a _    -> a
