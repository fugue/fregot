-- | Specialized Logic/List monad with two kinds of elements and error handling.
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
module Fregot.Eval.Bistream
    ( Bistream (..)
    , toErrorOrList
    , filter
    , collapse
    , null
    ) where

import           Data.Bifunctor (Bifunctor (..))
import           Data.Semigroup (Semigroup (..))
import           Prelude        hiding (filter, null)

--------------------------------------------------------------------------------
-- Streamy

data Bistream e i a
    = Done
    | Error   e
    | LCons   i (Bistream e i a)
    | RCons   a (Bistream e i a)
    | RSingle a  -- Relatively important optimization.
    deriving (Foldable, Functor, Traversable)

instance Bifunctor (Bistream e) where
    second  = fmap
    first f = \case
        Done      -> Done
        Error e   -> Error e
        LCons i b -> LCons (f i) (first f b)
        RCons x b -> RCons x     (first f b)
        RSingle x -> RSingle x

bistreamEmpty :: Bistream e i a
bistreamEmpty = Done

bistreamAppend :: Bistream e i a -> Bistream e i a -> Bistream e i a
bistreamAppend Done c           = c
bistreamAppend (Error e) _      = Error e
bistreamAppend (LCons i b) c    = LCons i (bistreamAppend b c)
bistreamAppend (RCons x b) c    = RCons x (bistreamAppend b c)
bistreamAppend (RSingle x) Done = RSingle x
bistreamAppend (RSingle x) c    = RCons x c

bistreamPure :: a -> Bistream e i a
bistreamPure = RSingle

bistreamAp :: Bistream e i (a -> b) -> Bistream e i a -> Bistream e i b
bistreamAp bf bx =
    bf `bistreamBind` \f -> bx `bistreamBind` \x -> bistreamPure (f x)

bistreamBind :: Bistream e i a -> (a -> Bistream e i b) -> Bistream e i b
bistreamBind Done _        = Done
bistreamBind (Error e) _   = Error e
bistreamBind (LCons i b) f = LCons i (bistreamBind b f)
bistreamBind (RCons x b) f = f x `bistreamAppend` bistreamBind b f
bistreamBind (RSingle x) f = f x

instance Semigroup (Bistream e i a) where
    (<>) = bistreamAppend

instance Monoid (Bistream e i a) where
    mempty  = bistreamEmpty
    mappend = bistreamAppend

instance Applicative (Bistream e i) where
    pure  = bistreamPure
    (<*>) = bistreamAp

instance Monad (Bistream e i) where
    (>>=) = bistreamBind

toErrorOrList :: Bistream e i a -> Either e [a]
toErrorOrList Done        = Right []
toErrorOrList (Error e)   = Left e
toErrorOrList (LCons _ b) = toErrorOrList b
toErrorOrList (RCons x b) = (x :) <$> toErrorOrList b
toErrorOrList (RSingle x) = Right [x]

filter :: (a -> Bool) -> Bistream e i a -> Bistream e i a
filter f = go
  where
    go Done        = Done
    go (Error e)   = Error e
    go (LCons i b) = LCons i (go b)
    go (RCons x b) = if f x then RCons x (go b) else go b
    go (RSingle x) = if f x then RSingle x else Done

collapse :: Bistream e i a -> Bistream e i [a]
collapse = go []
  where
    go acc Done        = RSingle (reverse acc)
    go _   (Error e)   = Error e
    go acc (LCons i b) = LCons i (go acc b)
    go acc (RCons x b) = go (x : acc) b
    go acc (RSingle x) = RSingle (reverse (x : acc))

null :: Bistream e i a -> Bistream e i Bool
null Done        = RSingle True
null (Error e)   = Error e
null (LCons i b) = LCons i (null b)
null (RCons _ _) = RSingle False
null (RSingle _) = RSingle False
