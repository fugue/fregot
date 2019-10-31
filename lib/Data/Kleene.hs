-- | Simple ternary Kleene logic
module Data.Kleene
    ( Ternary (..)
    , not
    , (&&), (||)
    , and, or
    , all, any
      -- Conversion to and from booleans.
    , fromTernary
    , fromBool
    , isTrue
    ) where

import           Data.Foldable (Foldable, foldr)
import qualified Prelude       as P

data Ternary = False | Unknown | True deriving (P.Eq, P.Show)

not :: Ternary -> Ternary
not False   = True
not Unknown = Unknown
not True    = False

(&&) :: Ternary -> Ternary -> Ternary
False   && _       = False
True    && r       = r
_       && False   = False
Unknown && Unknown = Unknown
Unknown && True    = Unknown
infixr 3 &&

(||) :: Ternary -> Ternary -> Ternary
True    || _       = True
False   || r       = r
_       || True    = True
Unknown || Unknown = Unknown
Unknown || False   = Unknown
infixr 2 ||

and :: Foldable f => f Ternary -> Ternary
and = foldr (&&) True

or :: Foldable f => f Ternary -> Ternary
or = foldr (||) False

all :: Foldable f => (a -> Ternary) -> f a -> Ternary
all f = foldr (\x -> (f x &&)) True

any :: Foldable f => (a -> Ternary) -> f a -> Ternary
any f = foldr (\x -> (f x ||)) False

fromTernary :: P.Bool -> Ternary -> P.Bool
fromTernary _ False   = P.False
fromTernary u Unknown = u
fromTernary _ True    = P.True

fromBool :: P.Bool -> Ternary
fromBool P.False = False
fromBool P.True  = True

isTrue :: Ternary -> P.Bool
isTrue = fromTernary P.False
