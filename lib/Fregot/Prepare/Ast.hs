{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Prepare.Ast
    ( RuleKind (..), _CompleteRule, _GenSetRule, _GenObjectRule, _FunctionRule
    , Rule (..), rulePackage, ruleName, ruleKey, ruleAnn, ruleDefault
    , ruleAssign, ruleKind, ruleInfo, ruleDefs
    , Rule'
    , RuleDefinition (..), ruleDefName, ruleDefImports, ruleDefAnn, ruleArgs
    , ruleIndex, ruleValue, ruleBodies, ruleElses
    , RuleElse (..), ruleElseAnn, ruleElseValue, ruleElseBody

    , Sugar.PackageName (..)
    , Sugar.packageNameFromString, Sugar.packageNameFromText

    , Imports
    , Sugar.ImportGut (..), Sugar._ImportInput, Sugar._ImportData
    , Sugar.Import (..), Sugar.importAnn, Sugar.importGut, Sugar.importAs

    , Sugar.Var, Sugar.unVar, Sugar.mkVar
    , Sugar.varToString, Sugar.varToText

    , RuleBody, Query
    , Literal (..), literalAnn, literalNegation, literalStatement, literalWith
    , Statement (..)
    , Comprehension (..)
    , _ArrayComp, _SetComp, _ObjectComp
    , IndexedComprehension (..), indexedUnique, indexedKeys, indexedAssignee
    , indexedComprehension
    , Term (..), _RefT, _CallT, _NameT, _ArrayT, _SetT, _ObjectT, _CompT
    , _ValueT
    , Object
    , Function (..), _OperatorFunction, _NamedFunction
    , BinOp (..)

    , Sugar.WithPath (..)
    , With (..), withAnn, withPath, withAs

    , Sugar.Scalar (..), Sugar._String, Sugar._Number, Sugar._Bool, Sugar._Null

      -- * Constructors and destructors
    , literal
    , unRefT
    , termToRule
    ) where

import           Control.Lens              (review, (^.))
import           Control.Lens.TH           (makeLenses, makePrisms)
import           Data.Hashable             (Hashable)
import qualified Data.List                 as L
import           Data.Unique               (Unique)
import           Fregot.Eval.Value         (Value)
import           Fregot.Names
import           Fregot.Names.Imports      (Imports)
import           Fregot.PrettyPrint        ((<$$>), (<+>), (<+>?), (?<+>))
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)
import qualified Fregot.Sugar              as Sugar
import           GHC.Generics              (Generic)

data RuleKind
    = CompleteRule
    | GenSetRule
    | GenObjectRule
    | FunctionRule Int  -- Arity.
    | ErrorRule         -- Rule that failed compilation.
    deriving (Eq, Show)

data Rule i a = Rule
    { _rulePackage :: !PackageName
    , _ruleName    :: !Var
    , _ruleKey     :: !Key
    , _ruleAnn     :: !SourceSpan
    , _ruleKind    :: !RuleKind
    , _ruleInfo    :: !i
    , _ruleDefault :: !(Maybe (Term a))
    , _ruleAssign  :: !Bool
    , _ruleDefs    :: [RuleDefinition a]
    } deriving (Functor, Show)

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
    } deriving (Functor, Show)

type RuleBody a = [Literal a]

type Query a = [Literal a]

data RuleElse a = RuleElse
    { _ruleElseAnn   :: !a
    , _ruleElseValue :: !(Maybe (Term a))
    , _ruleElseBody  :: !(RuleBody a)
    } deriving (Functor, Show)

data Literal a = Literal
    { _literalAnn       :: !a
    , _literalNegation  :: !Bool
    , _literalStatement :: !(Statement a)
    , _literalWith      :: ![With a]
    } deriving (Functor, Show)

data Statement a
    = UnifyS  a (Term a) (Term a)
    | AssignS a (Term a) (Term a)
    | TermS     (Term a)
    -- Indexed comprehension statements.  This is an optimization.
    | IndexedCompS a (IndexedComprehension a)
    deriving (Functor, Show)

data Comprehension a
    = ArrayComp  (Term a) (RuleBody a)
    | SetComp    (Term a) (RuleBody a)
    | ObjectComp (Term a) (Term a) (RuleBody a)
    deriving (Functor, Show)

data IndexedComprehension a = IndexedComprehension
    { _indexedUnique        :: !Unique
    , _indexedKeys          :: ![Var]
    , _indexedAssignee      :: !UnqualifiedVar
    , _indexedComprehension :: !(Comprehension a)
    } deriving (Functor, Show)

data Term a
    = RefT        a (Term a) (Term a)
    | CallT       a Function [Term a]
    | NameT       a Name
    | ArrayT      a [Term a]
    | SetT        a [Term a]
    | ObjectT     a (Object a)
    | CompT       a (Comprehension a)
    | ValueT      a Value
    | ErrorT      a
    deriving (Functor, Show)

data Function
    = NamedFunction Name
    | OperatorFunction BinOp
    deriving (Eq, Generic, Show)

instance Hashable Function

type Object a = [(Term a, Term a)]

data BinOp
    = NotEqualO
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
    , _withPath :: Sugar.WithPath
    , _withAs   :: !(Term a)
    } deriving (Functor, Show)

$(makePrisms ''RuleKind)
$(makePrisms ''Function)
$(makeLenses ''Rule)
$(makeLenses ''RuleDefinition)
$(makeLenses ''RuleElse)
$(makeLenses ''Literal)
$(makeLenses ''With)
$(makeLenses ''IndexedComprehension)
$(makePrisms ''Comprehension)
$(makePrisms ''Term)

--------------------------------------------------------------------------------
-- NOTE(jaspervdj): These instances are pretty much copied from the sugared
-- ones.  Not much we can do about that, though.  They are mostly used for
-- debugging (i.e. dump).

instance PP.Pretty PP.Sem (Rule i a) where
    pretty r = PP.vcat $
        (case r ^. ruleDefault of Nothing -> []; Just d -> [prettyDefault d]) ++
        map PP.pretty (r ^. ruleDefs)
      where
        prettyDefault d =
            PP.keyword "default" <+> PP.pretty (r ^. ruleName) <+> "=" <+>
            PP.pretty d

instance PP.Pretty PP.Sem (RuleDefinition a) where
    pretty rdef =
        PP.pretty (rdef ^. ruleDefName) <+>?
        (fmap (PP.parens . PP.commaSep . map PP.pretty) $ rdef ^. ruleArgs) <+>?
        (fmap (PP.brackets . PP.pretty) $ rdef ^. ruleIndex) <+>?
        (fmap (("=" <+>) . PP.pretty) $ rdef ^. ruleValue) <+>?
        (case rdef ^. ruleBodies of
            [] -> Nothing
            bs -> Just $ PP.hcat $ L.intersperse " " $ map prettyBody bs) <+>?
        (case rdef ^. ruleElses of
            [] -> Nothing
            es -> Just $ PP.hcat $ L.intersperse " " $ map prettyElse es)
      where
        prettyBody b = PP.punctuation "{" <$$>
            PP.ind (PP.vcat $ map PP.pretty b) <$$>
            PP.punctuation "}"
        prettyElse e =
            PP.keyword "else" <+>? fmap PP.pretty (e ^. ruleElseValue) <+>
            prettyBody (e ^. ruleElseBody)

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

    pretty (IndexedCompS _ (IndexedComprehension _ keys v c)) =
        PP.punctuation "[" <> PP.keyword "index" <+>
        PP.commaSep (map PP.pretty keys) <> PP.punctuation "]" <+>
        PP.pretty v <+> PP.punctuation ":=" <+> PP.pretty c

instance PP.Pretty PP.Sem (Comprehension a) where
    pretty (ArrayComp x lits) =
        PP.punctuation "[" <> PP.pretty x <+> PP.punctuation "|" <$$>
        PP.ind (PP.vcat $ map PP.pretty lits) <$$>
        PP.punctuation "]"
    pretty (SetComp x lits) =
        PP.punctuation "{" <> PP.pretty x <+> PP.punctuation "|" <$$>
        PP.ind (PP.vcat $ map PP.pretty lits) <$$>
        PP.punctuation "}"
    pretty (ObjectComp k x lits) =
        PP.punctuation "{" <> PP.pretty k <> PP.punctuation ":" <+>
        PP.pretty x <+> PP.punctuation "|" <$$>
        PP.ind (PP.vcat $ map PP.pretty lits) <$$>
        PP.punctuation "}"

instance PP.Pretty PP.Sem (Term a) where
    pretty (RefT _ x k) = PP.pretty x <> PP.punctuation "." <> PP.pretty k
    pretty (CallT _ f as)  =
        PP.pretty f <>
        PP.punctuation "(" <>
        PP.commaSep (map PP.pretty as) <>
        PP.punctuation ")"

    pretty (NameT _ v)       = PP.pretty v

    pretty (ArrayT _ a)      = PP.array a
    pretty (SetT _ s)        = PP.set s
    pretty (ObjectT _ o)     = PP.object o

    pretty (CompT _ o) = PP.pretty o

    pretty (ValueT _ v) = PP.literal $ PP.pretty v

    pretty (ErrorT _) = PP.errorNode

instance PP.Pretty PP.Sem Function where
    pretty (NamedFunction    v) = PP.pretty v
    pretty (OperatorFunction o) = PP.pretty o

instance PP.Pretty PP.Sem BinOp where
    pretty = PP.punctuation . \case
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
        PP.pretty (with ^. withPath) <+>
        PP.keyword "as" <+> PP.pretty (with ^. withAs)

--------------------------------------------------------------------------------
-- Constructor-like things

literal :: a -> Statement a -> Literal a
literal a s = Literal a False s []

unRefT :: Term a -> Maybe (Term a, [Term a])
unRefT (RefT _ l k) = case unRefT l of
    Nothing       -> Just (l, [k])
    Just (l', ks) -> Just (l', ks ++ [k])
unRefT _ = Nothing

termToRule
    :: SourceSpan -> PackageName -> Var -> Term SourceSpan -> Rule () SourceSpan
termToRule source pkgname var t = Rule
    { _rulePackage = pkgname
    , _ruleName    = var
    , _ruleKey     = review qualifiedVarFromKey (pkgname, var)
    , _ruleAnn     = source
    , _ruleKind    = CompleteRule
    , _ruleInfo    = ()
    , _ruleDefault = Nothing
    , _ruleAssign  = False
    , _ruleDefs    = pure RuleDefinition
        { _ruleDefName    = var
        , _ruleDefImports = mempty
        , _ruleDefAnn     = source
        , _ruleArgs       = Nothing
        , _ruleIndex      = Nothing
        , _ruleValue      = Just t
        , _ruleBodies     = []
        , _ruleElses      = []
        }
    }
