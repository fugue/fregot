{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Fregot.Eval.Tree where

import qualified Data.HashMap.Strict as HMS
import           Fregot.Names        (Var)

-- | TODO (jaspervdj): Move this further up the compilation pipeline, probably
-- up to the prepare step.
data Tree a
    = Branch (Maybe a) (HMS.HashMap Var (Tree a))
    deriving (Foldable, Functor, Show, Traversable)
