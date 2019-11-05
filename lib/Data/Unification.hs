{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
module Data.Unification
    ( MonadUnify (..)

    , lookup
    , bindVar
    , bindTerm

    , Unification
    , empty
    , lookupMaybe
    , keys
    ) where

import           Data.Hashable       (Hashable (..))
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashSet        as HS
import qualified Fregot.PrettyPrint  as PP
import           Prelude             hiding (lookup)

class Monad m => MonadUnify v t m | m -> v, m -> t where
    -- Actual unification that traverses terms.
    --
    -- TODO(jaspervdj): This should return a new `t`, so we can e.g. accumulate
    -- type origins.
    unify :: t -> t -> m ()

    -- State management.  Doing this through requiring MonadState and just
    -- having a Lens would be a bit nicer.
    getUnification    :: m (Unification v t)
    putUnification    :: Unification v t -> m ()
    modifyUnification :: (Unification v t -> Unification v t) -> m ()

lookup :: (Eq v, Hashable v, MonadUnify v t m) => v -> m (Maybe t)
lookup v = do
    uni0 <- getUnification
    return $ snd $ root v uni0

bindVar :: (Eq v, Hashable v, MonadUnify v t m) => v -> v -> m ()
bindVar x y = do
    uni0 <- getUnification
    let (xr, mxv) = root x uni0
        (yr, myv) = root y uni0
    if xr == yr
        then return ()
        else do
            mv <- case (mxv, myv) of
                (Nothing, Nothing) -> return Nothing
                (Just xv, Nothing) -> return (Just xv)
                (Nothing, Just yv) -> return (Just yv)
                (Just xv, Just yv) -> unify xv yv >> return (Just xv)

            modifyUnification $ \(Unification m) ->
                Unification $ HMS.insert xr (Ref yr) $ case mv of
                    Nothing -> m
                    Just v  -> HMS.insert yr (Root v) m

bindTerm :: (Eq v, Hashable v, MonadUnify v t m) => v -> t -> m ()
bindTerm v term = do
    -- TODO: Occurs check?
    uni0 <- getUnification
    case root v uni0 of
        (r, Nothing) -> modifyUnification $ unsafeInsert r term
        (_, Just t)  -> unify term t

data Node k a = Ref !k | Root !a deriving (Functor, Show)

newtype Unification k a = Unification (HMS.HashMap k (Node k a))
    deriving (Functor, Show)

empty :: Unification k a
empty = Unification HMS.empty

root
    :: (Eq k, Hashable k)
    => k -> Unification k a -> (k, Maybe a)
root k dj@(Unification m) = case HMS.lookup k m of
    Nothing       -> (k, Nothing)
    Just (Ref l)  -> root l dj
    Just (Root a) -> (k, Just a)

unsafeInsert
    :: (Eq k, Hashable k)
    => k -> t -> Unification k t -> Unification k t
unsafeInsert k t (Unification m) = Unification (HMS.insert k (Root t) m)

lookupMaybe :: (Eq k, Hashable k) => k -> Unification k a -> Maybe a
lookupMaybe v = snd . root v

keys :: (Eq k, Hashable k) => Unification k a -> HS.HashSet k
keys (Unification m) = HMS.keysSet m

instance (PP.Pretty PP.Sem k, PP.Pretty PP.Sem a) =>
        PP.Pretty PP.Sem (Unification k a) where
    pretty (Unification m) = PP.object
        [ ( k
          , case node of
                Root a -> PP.pretty' a
                Ref  r -> "->" <> PP.pretty' r
          )
        | (k, node) <- HMS.toList m
        ]
