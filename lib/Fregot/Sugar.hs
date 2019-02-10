{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Fregot.Sugar
    ( Var (..)
    , Rule (..), name
    , Expr (..)
    , Term (..)
    , Scalar (..)
    , Object
    , ObjectKey (..)
    ) where

import           Control.Lens       ((^.))
import           Control.Lens.TH    (makeLenses)
import           Data.Scientific    (Scientific)
import qualified Data.Scientific    as Scientific
import qualified Data.Text          as T
import           Fregot.PrettyPrint ((<+>))
import qualified Fregot.PrettyPrint as PP

newtype Var = Var {unVar :: T.Text}
    deriving (Show)

-- TODO:
-- * default
data Rule a = Rule
    { _name :: !Var
    } deriving (Show)

data Expr a
    = Term a (Term a)
    deriving (Show)

data Term a
    = VarT    a Var
    | ScalarT a (Scalar a)
    | ArrayT  a [Term a]
    | ObjectT a (Object a)
    deriving (Show)

data Scalar a
    = String T.Text
    | Number Scientific
    deriving (Show)

type Object a = [(ObjectKey a, Term a)]

data ObjectKey a
    = ScalarK a (Scalar a)
    deriving (Show)

$(makeLenses ''Rule)

instance PP.Pretty a Var where
    pretty = PP.pretty . unVar

instance PP.Pretty PP.Sem (Rule a) where
    pretty r = PP.pretty (r ^. name)

instance PP.Pretty PP.Sem (Expr a) where
    pretty (Term _ t) = PP.pretty t

instance PP.Pretty PP.Sem (Term a) where
    pretty (VarT _ v) = PP.pretty v
    pretty (ScalarT _ s) = PP.pretty s
    pretty (ArrayT _ a) = PP.parensSepVert
        (PP.punctuation "[") (PP.punctuation "]") (PP.punctuation ",")
        (map PP.pretty a)
    pretty (ObjectT _ o) = PP.parensSepVert
        (PP.punctuation "{") (PP.punctuation "}") (PP.punctuation ",")
        [ PP.pretty k <> PP.punctuation ":" <+> PP.pretty t
        | (k, t) <- o
        ]

instance PP.Pretty PP.Sem (Scalar a) where
    pretty (String s) = PP.literal $ PP.pretty $ show $ T.unpack s
    pretty (Number s) = PP.literal $ case Scientific.floatingOrInteger s of
        Left  x -> PP.pretty (x :: Double)
        Right x -> PP.pretty (x :: Integer)

instance PP.Pretty PP.Sem (ObjectKey a) where
    pretty (ScalarK _ s) = PP.pretty s
