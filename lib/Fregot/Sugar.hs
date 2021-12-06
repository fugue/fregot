{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Sugar
    ( PackageName (..), unPackageName, mkPackageName
    , packageNameFromString, packageNameFromText
    , dataPackageNameFromString

    , Modules
    , Module (..), moduleAnn, modulePackage, moduleImports, modulePolicy

    , ImportGut (..), _ImportInput, _ImportData
    , Import (..), importAnn, importGut, importAs

    , Var, unVar, mkVar
    , varToString, varToText
    , varFromText

    , Rule (..), ruleHead, ruleBodies
    , RuleHead (..), ruleAnn, ruleDefault, ruleName, ruleArgs, ruleIndex
    , ruleAssign, ruleValue, ruleElses
    , RuleBody, Query
    , RuleElse (..), ruleElseAnn, ruleElseValue, ruleElseBody
    , RuleStatement (..), _VarDeclS, _LiteralS
    , Literal (..), literalAnn, literalNegation, literalExpr, literalWith

    , Expr (..), exprAnn
    , Term (..), termAnn
    , RefArg (..)
    , Scalar (..), _String, _Number, _Bool, _Null
    , Object
    , ObjectKey (..), _ScalarK, _VarK, _RefK, _ErrorK
    , BinOp (..)
    , WithPath (..)
    , With (..), withAnn, withPath, withAs

    , Nested (..)
    , nestedToString

    , moduleRuleNames
    , moduleDependencies
    , termFromExpr
    , exprFromLiteral
    , literalFromQuery
    ) where

import           Control.Lens       (Fold, Lens', lens, preview, (^.), _2)
import           Control.Lens.Prism (Prism', prism')
import           Control.Lens.TH    (makeLenses, makePrisms)
import           Control.Monad      (guard)
import           Data.Binary        (Binary)
import           Data.Hashable      (Hashable)
import qualified Data.List          as L
import           Data.Scientific    (Scientific)
import qualified Data.Text.Extended as T
import           Fregot.Names
import           Fregot.PrettyPrint ((<$$>), (<+>), (<+>?), (?<+>))
import qualified Fregot.PrettyPrint as PP
import           GHC.Generics       (Generic)

-- | Once can load multiple modules under the same package name.
type Modules a n = [Module a n]

data Module a n = Module
    { _moduleAnn     :: !a
    , _modulePackage :: !PackageName
    , _moduleImports :: ![Import a]
    , _modulePolicy  :: ![Rule a n]
    } deriving (Generic, Show)

instance (Binary a, Binary n) => Binary (Module a n)

data ImportGut
    = ImportInput !PackageName
    | ImportData  !PackageName
    deriving (Generic, Show)

instance Binary ImportGut

data Import a = Import
    { _importAnn :: !a
    , _importGut :: !ImportGut
    , _importAs  :: !(Maybe UnqualifiedVar)
    } deriving (Generic, Show)

instance Binary a => Binary (Import a)

data Rule a n = Rule
    { _ruleHead   :: !(RuleHead a n)
    , _ruleBodies :: ![RuleBody a n]
    , _ruleElses  :: ![RuleElse a n]
    } deriving (Generic, Show)

instance (Binary a, Binary n) => Binary (Rule a n)

data RuleHead a n = RuleHead
    { _ruleAnn     :: !a
    , _ruleDefault :: !Bool
    , -- | If a rule is declared using `:=`, we consider it "assigned", and we
      -- will only allow a single definition.
      _ruleAssign  :: !Bool
    , _ruleName    :: !UnqualifiedVar
    , _ruleArgs    :: !(Maybe [Term a n])
    , _ruleIndex   :: !(Maybe (Term a n))
    , _ruleValue   :: !(Maybe (Expr a n))
    } deriving (Generic, Show)

instance (Binary a, Binary n) => Binary (RuleHead a n)

type RuleBody a n = [RuleStatement a n]

type Query a n = RuleBody a n

data RuleElse a n = RuleElse
    { _ruleElseAnn   :: !a
    , _ruleElseValue :: !(Maybe (Term a n))
    , _ruleElseBody  :: !(RuleBody a n)
    } deriving (Generic, Show)

instance (Binary a, Binary n) => Binary (RuleElse a n)

data RuleStatement a n
    = LiteralS !(Literal a n)
    | VarDeclS !a ![UnqualifiedVar]
    deriving (Generic, Show)

instance (Binary a, Binary n) => Binary (RuleStatement a n)

data Literal a n = Literal
    { _literalAnn      :: !a
    , _literalNegation :: !Bool
    , _literalExpr     :: !(Expr a n)
    , _literalWith     :: ![With a n]
    } deriving (Generic, Show)

instance (Binary a, Binary n) => Binary (Literal a n)

data Expr a n
    = TermE   a (Term a n)
    -- NOTE(jaspervdj): Perhaps assign should be part of 'Literal'.  It is
    -- missing from the grammar, so I'm not sure at this point.
    | BinOpE  a (Expr a n) BinOp (Expr a n)
    | ParensE a (Expr a n)
    -- Indirect references.  Can be folded into 'RefT' but it's useful to
    -- do it this way for renaming, as we don't attempt to rename indirect
    -- references.
    | IndRefE a (Term a n) [RefArg a n]
    deriving (Generic, Show)

instance (Binary a, Binary n) => Binary (Expr a n)

data Term a n
    = RefT      a a n [RefArg a n]
    | CallT     a [n] [Expr a n]
    | VarT      a n
    | ScalarT   a Scalar

    | ArrayT    a [Expr a n]
    | SetT      a [Expr a n]
    | ObjectT   a (Object a n)

    | ArrayCompT  a (Term a n) (RuleBody a n)
    | SetCompT    a (Term a n) (RuleBody a n)
    | ObjectCompT a (ObjectKey a n) (Term a n) (RuleBody a n)

    | ErrorT a
    deriving (Generic, Show)

instance (Binary a, Binary n) => Binary (Term a n)

data RefArg a n
    = RefBrackArg !(Expr a n)
    | RefDotArg !a !UnqualifiedVar
    deriving (Generic, Show)

instance (Binary a, Binary n) => Binary (RefArg a n)

data Scalar
    = String T.Text
    | Number Scientific
    | Bool   Bool
    | Null
    deriving (Eq, Generic, Ord, Show)

instance Binary Scalar
instance Hashable Scalar

type Object a n = [(ObjectKey a n, Expr a n)]

data ObjectKey a n
    = ScalarK a Scalar
    | VarK    a UnqualifiedVar
    | RefK    a n [RefArg a n]
    | ErrorK  a
    deriving (Generic, Show)

instance (Binary a, Binary n) => Binary (ObjectKey a n)

data BinOp
    = UnifyO
    | AssignO
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
    deriving (Generic, Show)

instance Binary BinOp

data WithPath
    = InputWithPath [UnqualifiedVar]
    | DataWithPath  [UnqualifiedVar]
    deriving (Generic, Show)

instance Binary WithPath

data With a n = With
    { _withAnn  :: !a
    , _withPath :: !WithPath
    , _withAs   :: !(Term a n)
    } deriving (Generic, Show)

instance (Binary a, Binary n) => Binary (With a n)

$(makePrisms ''ImportGut)
$(makeLenses ''Import)
$(makeLenses ''Module)
$(makeLenses ''Rule)
$(makeLenses ''RuleHead)
$(makeLenses ''RuleElse)
$(makePrisms ''RuleStatement)
$(makeLenses ''Literal)
$(makeLenses ''With)
$(makePrisms ''Expr)
$(makePrisms ''Scalar)
$(makePrisms ''ObjectKey)

instance PP.Pretty PP.Sem ImportGut where
    pretty (ImportData pkg) =
        PP.keyword "data" <> PP.punctuation "." <> PP.pretty pkg
    pretty (ImportInput pkg) =
        PP.keyword "input" <> PP.punctuation "." <> PP.pretty pkg

instance PP.Pretty PP.Sem (Import a) where
    pretty imp =
        PP.keyword "import" <+> PP.pretty (imp ^. importGut) <+>?
        fmap PP.pretty (imp ^. importAs)

instance PP.Pretty PP.Sem n => PP.Pretty PP.Sem (Module a n) where
    pretty pkg = PP.vcat2 $
        (PP.keyword "package" <+> PP.pretty (pkg ^. modulePackage)) :
        (case pkg ^. moduleImports of
            []   -> []
            imps -> [PP.vcat $ map PP.pretty imps]) ++
        map PP.pretty (pkg ^. modulePolicy)

prettyRuleBody :: PP.Pretty PP.Sem n => RuleBody a n -> PP.SemDoc
prettyRuleBody bs =
    PP.punctuation "{" <$$>
    PP.ind (PP.vcat $ map (\b -> PP.pretty b) bs) <$$>
    PP.punctuation "}"

instance PP.Pretty PP.Sem n => PP.Pretty PP.Sem (Rule a n) where
    pretty r =
        PP.pretty (r ^. ruleHead) <+>?
        (case r ^. ruleBodies of
            [] -> Nothing
            bs -> Just $ mconcat $ L.intersperse " " $ map prettyRuleBody bs) <+>?
        (case r ^. ruleElses of
            [] -> Nothing
            es -> Just $ mconcat $ L.intersperse " " $ map PP.pretty es)

instance PP.Pretty PP.Sem n => PP.Pretty PP.Sem (RuleHead a n) where
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

instance PP.Pretty PP.Sem n => PP.Pretty PP.Sem (RuleElse a n) where
    pretty re =
        PP.keyword "else" <+>
        (case re ^. ruleElseValue of
            Nothing -> Nothing
            Just v  -> Just $ PP.punctuation "=" <+> PP.pretty v) ?<+>
        prettyRuleBody (re ^. ruleElseBody)

instance PP.Pretty PP.Sem n => PP.Pretty PP.Sem (RuleStatement a n) where
    pretty (LiteralS l)    = PP.pretty l
    pretty (VarDeclS _ vs) =
        PP.keyword "some" <+> PP.commaSep (map PP.pretty vs)

instance PP.Pretty PP.Sem n => PP.Pretty PP.Sem (Literal a n) where
    pretty lit =
        (if lit ^. literalNegation
            then Just (PP.keyword "not")
            else Nothing) ?<+>
        PP.pretty (lit ^. literalExpr) <+>?
        (case lit ^. literalWith of
            []    -> Nothing
            withs -> Just $ PP.hcat (L.intersperse " " (map PP.pretty withs)))

instance PP.Pretty PP.Sem n => PP.Pretty PP.Sem (Expr a n) where
    pretty (TermE _ t)      = PP.pretty t
    pretty (BinOpE _ x o y) = PP.pretty x <+> PP.pretty o <+> PP.pretty y
    pretty (ParensE _ e)    =
        PP.punctuation "(" <> PP.pretty e <> PP.punctuation ")"
    pretty (IndRefE _ x ys) = PP.pretty x <> mconcat (map PP.pretty ys)

instance PP.Pretty PP.Sem n => PP.Pretty PP.Sem (Term a n) where
    pretty (RefT _ _ v args) = PP.pretty v <> mconcat (map PP.pretty args)
    pretty (CallT _ vs as)  =
        PP.pretty (Nested vs) <>
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

    pretty (ErrorT _) = PP.errorNode

prettyComprehensionBody :: PP.Pretty PP.Sem n => RuleBody a n -> PP.SemDoc
prettyComprehensionBody lits = mconcat $ L.intersperse
    (PP.punctuation ";" <> PP.space)
    (map PP.pretty lits)

instance PP.Pretty PP.Sem n => PP.Pretty PP.Sem (RefArg a n) where
    pretty (RefBrackArg a) =
        PP.punctuation "[" <> PP.pretty a <> PP.punctuation "]"
    pretty (RefDotArg _ k) =
        PP.punctuation "." <> PP.pretty k

instance PP.Pretty PP.Sem Scalar where
    pretty (String s) = PP.literal $ PP.pretty $ show $ T.unpack s
    pretty (Number s) = PP.literal $ PP.pretty s
    pretty (Bool   b) = PP.literal $ if b then "true" else "false"
    pretty Null       = PP.literal "null"

instance PP.Pretty PP.Sem n => PP.Pretty PP.Sem (ObjectKey a n) where
    pretty (ScalarK _ s) = PP.pretty s
    pretty (VarK _ v)    = PP.pretty v
    pretty (RefK _ v a)  = PP.pretty v <> mconcat (map PP.pretty a)
    pretty (ErrorK _)    = PP.errorNode

instance PP.Pretty PP.Sem BinOp where
    pretty = PP.punctuation . \case
        UnifyO              -> "="
        AssignO             -> ":="
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

instance PP.Pretty PP.Sem WithPath where
    pretty = \case
        InputWithPath p -> PP.keyword "input" <> PP.punctuation "." <> path p
        DataWithPath  p -> PP.keyword "data"  <> PP.punctuation "." <> path p
      where
        path = mconcat . L.intersperse (PP.punctuation ".") . map PP.pretty

instance PP.Pretty PP.Sem n => PP.Pretty PP.Sem (With a n) where
    pretty with = PP.keyword "with" <+>
        PP.pretty (with ^. withPath) <+>
        PP.keyword "as" <+> PP.pretty (with ^. withAs)

exprAnn :: Lens' (Expr a n) a
exprAnn = lens getAnn setAnn
  where
    getAnn = \case
        TermE   a _     -> a
        BinOpE  a _ _ _ -> a
        ParensE a _     -> a
        IndRefE a _ _   -> a

    setAnn e a = case e of
        TermE   _ t     -> TermE   a t
        BinOpE  _ x o y -> BinOpE  a x o y
        ParensE _ x     -> ParensE a x
        IndRefE _ x ys  -> IndRefE a x ys

termAnn :: Lens' (Term a n) a
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
        ErrorT      a       -> a

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
        ErrorT      _       -> ErrorT      a

moduleRuleNames :: Fold (Module a n) Var
moduleRuleNames = modulePolicy . traverse . ruleHead . ruleName

moduleDependencies :: Fold (Module a n) PackageName
moduleDependencies = moduleImports . traverse . importGut . _ImportData

termFromExpr :: Prism' (Expr a n) (Term a n)
termFromExpr = prism' (\t -> TermE (t ^. termAnn) t) (preview (_TermE . _2))

exprFromLiteral :: Prism' (Literal a n) (Expr a n)
exprFromLiteral = prism' fromExpr toExpr
  where
    toExpr lit = do
        guard $ lit ^. literalNegation == False
        guard $ null $ lit ^. literalWith
        pure $ lit ^. literalExpr

    fromExpr e = Literal
        { _literalAnn      = e ^. exprAnn
        , _literalNegation = False
        , _literalExpr     = e
        , _literalWith     = []
        }

literalFromQuery :: Prism' (Query a n) (Literal a n)
literalFromQuery = prism' toLiteral fromLiteral
  where
    toLiteral lit = [LiteralS lit]
    fromLiteral query = case query of
        [LiteralS lit] -> Just lit
        _              -> Nothing
