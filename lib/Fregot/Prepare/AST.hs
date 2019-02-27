{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Prepare.AST
    ( RuleKind (..)
    , Rule (..), ruleName, ruleAnn, ruleDefault, ruleKind, ruleDefs
    , RuleDefinition (..), ruleDefName, ruleDefImports, ruleDefAnn, ruleArgs
    , ruleIndex, ruleValue, ruleBody

    , Sugar.PackageName (..)
    , Sugar.packageNameToString, Sugar.packageNameToText

    , Sugar.Import (..), Sugar.importAnn, Sugar.importPackage, Sugar.importAs

    , Sugar.Var (..)
    , Sugar.varToString, Sugar.varToText

    , RuleBody
    , Literal (..), literalNegation, literalStatement, literalWith
    , Statement (..)
    , Expr (..), exprAnn
    , Term (..), termAnn
    , BinOp (..)

    , Sugar.Scalar (..)
    ) where

import           Control.Lens              (Lens', lens)
import           Control.Lens.TH           (makeLenses)
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Fregot.Sugar              (Var (..))
import qualified Fregot.Sugar              as Sugar

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
    , _ruleDefImports :: ![Sugar.Import a]
    , _ruleDefAnn     :: !a
    , _ruleArgs       :: !(Maybe [Term a])
    , _ruleIndex      :: !(Maybe (Term a))
    , _ruleValue      :: !(Maybe (Term a))
    , _ruleBody       :: !(RuleBody a)
    } deriving (Show)

type RuleBody a = [Literal a]

data Literal a = Literal
    { _literalNegation  :: !Bool
    , _literalStatement :: !(Statement a)
    , _literalWith      :: ![Sugar.With a]
    } deriving (Show)

data Statement a
    = UnifyS  a (Expr a) (Expr a)
    | AssignS a Var (Expr a)
    | ExprS   (Expr a)
    deriving (Show)

data Expr a
    = TermE   a (Term a)
    | BinOpE  a (Expr a) BinOp (Expr a)
    deriving (Show)

data Term a
    = RefT        a (Term a) (Term a)
    | CallT       a [Var] [Term a]
    | VarT        a Var
    | ScalarT     a (Sugar.Scalar a)
    | ArrayT      a [Expr a]
    | SetT        a [Expr a]
    | ObjectT     a (Object a)
    | ArrayCompT  a (Term a) (RuleBody a)
    | SetCompT    a (Term a) (RuleBody a)
    | ObjectCompT a (Term a) (Term a) (RuleBody a)
    deriving (Show)

type Object a = [(Term a, Expr a)]

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
    deriving (Show)

$(makeLenses ''Rule)
$(makeLenses ''RuleDefinition)
$(makeLenses ''Literal)

exprAnn :: Lens' (Expr a) a
exprAnn = lens getAnn setAnn
  where
    getAnn = \case
        TermE   a _     -> a
        BinOpE  a _ _ _ -> a

    setAnn e a = case e of
        TermE   _ t     -> TermE   a t
        BinOpE  _ x o y -> BinOpE  a x o y

termAnn :: Lens' (Term a) a
termAnn = lens getAnn setAnn
  where
    getAnn = \case
        RefT        a _ _   -> a
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
        RefT        _ x k   -> RefT        a x k
        CallT       _ f as  -> CallT       a f as
        VarT        _ v     -> VarT        a v
        ScalarT     _ s     -> ScalarT     a s
        ArrayT      _ l     -> ArrayT      a l
        SetT        _ s     -> SetT        a s
        ObjectT     _ o     -> ObjectT     a o
        ArrayCompT  _ x b   -> ArrayCompT  a x b
        SetCompT    _ x b   -> SetCompT    a x b
        ObjectCompT _ k x b -> ObjectCompT a k x b
