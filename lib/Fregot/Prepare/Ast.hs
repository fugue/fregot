{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Prepare.Ast
    ( RuleKind (..)
    , Rule (..), ruleName, ruleAnn, ruleDefault, ruleKind, ruleDefs
    , RuleDefinition (..), ruleDefName, ruleDefImports, ruleDefAnn, ruleArgs
    , ruleIndex, ruleValue, ruleBodies, ruleElses
    , RuleElse (..), ruleElseAnn, ruleElseValue, ruleElseBody

    , Sugar.PackageName (..)
    , Sugar.packageNameFromString, Sugar.packageNameFromText

    , Sugar.Import (..), Sugar.importAnn, Sugar.importPackage, Sugar.importAs

    , Sugar.Var (..)
    , Sugar.varToString, Sugar.varToText

    , Imports
    , RuleBody
    , Literal (..), literalAnn, literalNegation, literalStatement, literalWith
    , Statement (..)
    , Term (..)
    , Object
    , Function (..)
    , BinOp (..)

    , With (..), withAnn, withPath, withAs

    , Sugar.Scalar (..)

    , Sugar.NestedVar (..)

      -- * Constructors
    , literal
    ) where

import           Control.Lens.TH           (makeLenses)
import           Data.Hashable             (Hashable)
import qualified Data.HashMap.Strict       as HMS
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Fregot.Sugar              (Var (..))
import qualified Fregot.Sugar              as Sugar
import           GHC.Generics              (Generic)

data RuleKind
    = CompleteRule
    | GenSetRule
    | GenObjectRule
    | FunctionRule
    deriving (Eq, Show)

data Rule a = Rule
    { _ruleName    :: !Var
    , _ruleAnn     :: !SourceSpan
    , _ruleDefault :: !(Maybe (Term a))
    , _ruleKind    :: !RuleKind
    , _ruleDefs    :: [RuleDefinition a]
    } deriving (Show)

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

type Imports a = HMS.HashMap Var (a, Sugar.PackageName)

type RuleBody a = [Literal a]

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
    | AssignS a Var (Term a)
    | TermS     (Term a)
    deriving (Eq, Show)

data Term a
    = RefT        a (Term a) (Term a)
    | CallT       a Function [Term a]
    | VarT        a Var
    | ScalarT     a (Sugar.Scalar a)
    | ArrayT      a [Term a]
    | SetT        a [Term a]
    | ObjectT     a (Object a)
    | ArrayCompT  a (Term a) (RuleBody a)
    | SetCompT    a (Term a) (RuleBody a)
    | ObjectCompT a (Term a) (Term a) (RuleBody a)
    deriving (Eq, Show)

data Function
    = NamedFunction [Var]
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
    | BinOrO
    deriving (Eq, Generic, Show)

instance Hashable BinOp

data With a = With
    { _withAnn  :: !a
    , _withPath :: [Var]
    , _withAs   :: !(Term a)
    } deriving (Eq, Show)

$(makeLenses ''Rule)
$(makeLenses ''RuleDefinition)
$(makeLenses ''RuleElse)
$(makeLenses ''Literal)
$(makeLenses ''With)

instance PP.Pretty PP.Sem Function where
    pretty (NamedFunction    vs) = PP.pretty (Sugar.NestedVar vs)
    pretty (OperatorFunction o)  = PP.pretty o

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
        BinOrO              -> "|"

--------------------------------------------------------------------------------
-- Constructor-like things

literal :: a -> Statement a -> Literal a
literal a s = Literal a False s []
