{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Fregot.Eval.Number
    ( Number (..)
    , int
    , double
    , fromScientific
    ) where

import           Control.Lens       ((^.))
import           Control.Lens.Iso   (Iso', iso)
import           Control.Lens.Prism (Prism', prism')
import           Data.Hashable      (Hashable (..))
import           Data.Scientific    (Scientific)
import qualified Data.Scientific    as Scientific
import qualified Fregot.PrettyPrint as PP

data Number = I {-# UNPACK #-} !Int | D {-# UNPACK #-} !Double

int :: Prism' Number Int
int = prism' I toInt
  where
    toInt (I i) = Just i
    toInt (D d) =
        let i = floor d in if fromIntegral i == d then Just i else Nothing

double :: Iso' Number Double
double = iso (\n -> case n of I x -> fromIntegral x; D x -> x) D

instance Eq Number where
    (==) = homoBinOp (==) (==)
    {-# INLINE (==) #-}

instance Hashable Number where
    hashWithSalt s (I i) = hashWithSalt s (fromIntegral i :: Double)
    hashWithSalt s (D d) = hashWithSalt s d

instance Ord Number where
    compare = homoBinOp compare compare

instance Show Number where
    show (I i) = show i
    show (D d) = show d

instance Num Number where
    (+) = numBinOp (+) (+)
    (-) = numBinOp (-) (-)
    (*) = numBinOp (*) (*)

    abs (I i) = I (abs i)
    abs (D d) = D (abs d)
    signum (I i) = I (signum i)
    signum (D d) = D (signum d)
    fromInteger  = I . fromInteger

instance Fractional Number where
    x / y        = D (x ^. double / y ^. double)
    fromRational = D . fromRational

instance PP.Pretty a Number where
    pretty (I i) = PP.pretty i
    pretty (D d) = PP.pretty d

numBinOp
    :: (Int -> Int -> Int)
    -> (Double -> Double -> Double)
    -> Number -> Number -> Number
numBinOp f _ (I x) (I y) = I (f x y)
numBinOp _ g (I x) (D y) = D (g (fromIntegral x) y)
numBinOp _ g (D x) (I y) = D (g x (fromIntegral y))
numBinOp _ g (D x) (D y) = D (g x y)
{-# INLINE numBinOp #-}

homoBinOp
    :: (Int -> Int -> a)
    -> (Double -> Double -> a)
    -> Number -> Number -> a
homoBinOp f _ (I x) (I y) = f x y
homoBinOp _ g (I x) (D y) = g (fromIntegral x) y
homoBinOp _ g (D x) (I y) = g x (fromIntegral y)
homoBinOp _ g (D x) (D y) = g x y
{-# INLINE homoBinOp #-}

fromScientific :: Scientific -> Number
fromScientific = either D I . Scientific.floatingOrInteger
