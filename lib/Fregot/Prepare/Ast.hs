{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Prepare.Ast
    ( RuleKind (..), _CompleteRule, _GenSetRule, _GenObjectRule, _FunctionRule
    , Rule (..), rulePackage, ruleName, ruleAnn, ruleDefault, ruleKind, ruleInfo
    , ruleDefs
    , Rule'
    , RuleDefinition (..), ruleDefName, ruleDefImports, ruleDefAnn, ruleArgs
    , ruleIndex, ruleValue, ruleBodies, ruleElses
    , RuleElse (..), ruleElseAnn, ruleElseValue, ruleElseBody

    , Sugar.PackageName (..)
    , Sugar.packageNameFromString, Sugar.packageNameFromText

    , Sugar.Import (..), Sugar.importAnn, Sugar.importPackage, Sugar.importAs

    , Sugar.Var, Sugar.unVar, Sugar.mkVar
    , Sugar.varToString, Sugar.varToText

    , Imports
    , RuleBody, Query
    , Literal (..), literalAnn, literalNegation, literalStatement, literalWith
    , Statement (..)
    , Term (..)
    , Object
    , Function (..)
    , BinOp (..)

    , With (..), withAnn, withPath, withAs

    , Sugar.Scalar (..)

      -- * Constructors
    , literal


    , prettyComprehensionBody
    ) where

import           Control.Lens              ((^.))
import           Control.Lens.TH           (makeLenses, makePrisms)
import           Data.Hashable             (Hashable)
import qualified Data.List                 as L
import           Fregot.Names
import           Fregot.PrettyPrint        ((<+>), (<+>?), (?<+>))
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)
import qualified Fregot.Sugar              as Sugar
import           GHC.Generics              (Generic)

data RuleKind
    = CompleteRule
    | GenSetRule
    | GenObjectRule
    | FunctionRule Int  -- Arity.
    deriving (Eq, Show)

data Rule i a = Rule
    { _rulePackage :: !PackageName
    , _ruleName    :: !Var
    , _ruleAnn     :: !SourceSpan
    , _ruleKind    :: !RuleKind
    , _ruleInfo    :: !i
    , _ruleDefault :: !(Maybe (Term a))
    , _ruleDefs    :: [RuleDefinition a]
    } deriving (Show)

type Rule' = Rule () SourceSpan

data RuleDefinition a = RuleDefinition
    { _ruleDefName    :: !Var
    , _ruleDefImports :: !(Imports a)
    , _ruleDefAnn     :: !a
    , _ruleArgs       :: !(Maybe [Term a])
    , _ruleIndex      :: !(Maybe (Term a))
    , _ruleValue      :: !(Maybe (Term a))
    , _ruleBodies     :: ![RuleBody a]
    , _ruleElses      :: ![RuleElse a]
    } deriving (Show)

type RuleBody a = [Literal a]

type Query a = [Literal a]

data RuleElse a = RuleElse
    { _ruleElseAnn   :: !a
    , _ruleElseValue :: !(Maybe (Term a))
    , _ruleElseBody  :: !(RuleBody a)
    } deriving (Eq, Show)

data Literal a = Literal
    { _literalAnn       :: !a
    , _literalNegation  :: !Bool
    , _literalStatement :: !(Statement a)
    , _literalWith      :: ![With a]
    } deriving (Eq, Show)

data Statement a
    = UnifyS  a (Term a) (Term a)
    | AssignS a UnqualifiedVar (Term a)
    | TermS     (Term a)
    deriving (Eq, Show)

data Term a
    = RefT        a (Term a) (Term a)
    | CallT       a Function [Term a]
    | NameT       a Name
    | ScalarT     a Sugar.Scalar
    | ArrayT      a [Term a]
    | SetT        a [Term a]
    | ObjectT     a (Object a)
    | ArrayCompT  a (Term a) (RuleBody a)
    | SetCompT    a (Term a) (RuleBody a)
    | ObjectCompT a (Term a) (Term a) (RuleBody a)
    deriving (Eq, Show)

data Function
    = NamedFunction Name
    | OperatorFunction BinOp
    deriving (Eq, Generic, Show)

instance Hashable Function

type Object a = [(Term a, Term a)]

data BinOp
    = EqualO
    | NotEqualO
    | LessThanO
    | LessThanOrEqualO
    | GreaterThanO
    | GreaterThanOrEqualO
    | PlusO
    | MinusO
    | TimesO
    | DivideO
    | ModuloO
    | BinAndO
    | BinOrO
    deriving (Eq, Generic, Show)

instance Hashable BinOp

data With a = With
    { _withAnn  :: !a
    , _withPath :: [UnqualifiedVar]
    , _withAs   :: !(Term a)
    } deriving (Eq, Show)

$(makePrisms ''RuleKind)
$(makeLenses ''Rule)
$(makeLenses ''RuleDefinition)
$(makeLenses ''RuleElse)
$(makeLenses ''Literal)
$(makeLenses ''With)

--------------------------------------------------------------------------------
-- NOTE(jaspervdj): These instances are pretty much copied from the sugared
-- ones.  Not much we can do about that, though.

instance PP.Pretty PP.Sem (Literal a) where
    pretty lit =
        (if lit ^. literalNegation
            then Just (PP.keyword "not")
            else Nothing) ?<+>
        PP.pretty (lit ^. literalStatement) <+>?
        (case lit ^. literalWith of
            []    -> Nothing
            withs -> Just $ PP.hcat (L.intersperse " " (map PP.pretty withs)))

instance PP.Pretty PP.Sem (Statement a) where
    pretty (UnifyS  _ x y) = PP.pretty x <+> PP.punctuation "=" <+> PP.pretty y
    pretty (AssignS _ v x) = PP.pretty v <+> PP.punctuation ":=" <+> PP.pretty x
    pretty (TermS x)       = PP.pretty x

instance PP.Pretty PP.Sem (Term a) where
    pretty (RefT _ x k) = PP.pretty x <> PP.punctuation "." <> PP.pretty k
    pretty (CallT _ f as)  =
        PP.pretty f <>
        PP.punctuation "(" <>
        PP.commaSep (map PP.pretty as) <>
        PP.punctuation ")"

    pretty (NameT _ v)       = PP.pretty v
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

instance PP.Pretty PP.Sem Function where
    pretty (NamedFunction    v) = PP.pretty v
    pretty (OperatorFunction o) = PP.pretty o

instance PP.Pretty PP.Sem BinOp where
    pretty = PP.punctuation . \case
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
        ModuloO             -> "%"
        BinAndO             -> "&"
        BinOrO              -> "|"

instance PP.Pretty PP.Sem (With a) where
    pretty with = PP.keyword "with" <+>
        (mconcat $ L.intersperse
            (PP.punctuation ".")
            (map PP.pretty (with ^. withPath))) <+>
        PP.keyword "as" <+> PP.pretty (with ^. withAs)

--------------------------------------------------------------------------------
-- Constructor-like things

literal :: a -> Statement a -> Literal a
literal a s = Literal a False s []
