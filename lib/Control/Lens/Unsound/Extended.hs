{-# LANGUAGE Rank2Types #-}
module Control.Lens.Unsound.Extended
    ( module Control.Lens.Unsound
    , adjoin
    ) where

import           Control.Lens
import           Control.Lens.Unsound

-- | Taken from lens 4.18
adjoin :: Traversal' s a -> Traversal' s a -> Traversal' s a
adjoin t1 t2 =
    lensProduct (partsOf t1) (partsOf t2) . both . each
