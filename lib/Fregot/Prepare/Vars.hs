{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Prepare.Vars
    (
      {-
      Dir (..)
    , Vars (..), varsIn, varsOut

    , Scope (..), locals, globals, builtins
    , emptyScope

    , freeRuleBody
    , freeStatement
    , freeTerm
    -}

      Safe (..)
    , Arities
    , ovRuleBody
    ) where

import           Control.Lens          ((^.))
import           Data.Hashable         (Hashable)
import qualified Data.HashSet.Extended as HS
import           Data.List             (foldl')
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens

newtype Safe v = Safe {unSafe :: HS.HashSet v}
    deriving (Eq, Monoid, Semigroup, Show)

markSafe :: (Eq v, Hashable v) => v -> Safe v
markSafe = Safe . HS.singleton

markTermSafe :: Term a -> Safe Var
markTermSafe t = Safe (HS.toHashSetOf (termCosmosNoClosures . termVars) t)

isSafe :: (Eq v, Hashable v) => v -> Safe v -> Bool
isSafe v (Safe s) = HS.member v s

type Arities = Function -> Int

ovRuleBody :: Arities -> Safe Var -> RuleBody a -> Safe Var
ovRuleBody arities safe@(Safe initial) body =
    Safe $ total `HS.difference` initial
  where
    Safe total = foldl'
        (\acc lit -> let ov = ovLiteral arities acc lit in acc <> ov)
        safe
        body

ovLiteral :: Arities -> Safe Var -> Literal a -> Safe Var
ovLiteral arities safe lit
    | lit ^. literalNegation = mempty
    | otherwise              =
        ovStatement arities safe (lit ^. literalStatement)

ovStatement :: Arities -> Safe Var -> Statement a -> Safe Var
ovStatement arities safe = \case
    TermS t       -> ovTerm arities safe t
    AssignS _ v t -> markSafe v <> ovTerm arities safe t
    UnifyS _ x y  -> ovUnify arities safe x y

ovTerm :: Arities -> Safe Var -> Term a -> Safe Var
ovTerm arities safe (RefT _ x k) =
    ovTerm arities safe x <>
    markTermSafe k

ovTerm arities safe (CallT _ function args) =
    let arity = arities function in
    foldMap (ovTerm arities safe) (take arity args) <>
    foldMap markTermSafe (drop arity args)

ovTerm _arities _safe (VarT _ _) = mempty

ovTerm _arities _safe (ScalarT _ _) = mempty

ovTerm arities safe (ArrayT _ xs) = foldMap (ovTerm arities safe) xs

ovTerm arities safe (SetT _ xs) = foldMap (ovTerm arities safe) xs

ovTerm arities safe (ObjectT _ xs) =
    foldMap (\(k, v) -> ovTerm arities safe k <> ovTerm arities safe v) xs

ovTerm _arities _safe (ArrayCompT _ _ _) = mempty
ovTerm _arities _safe (SetCompT _ _ _) = mempty
ovTerm _arities _safe (ObjectCompT _ _ _ _) = mempty

ovUnify :: Arities -> Safe Var -> Term a -> Term a -> Safe Var
ovUnify _arities safe (VarT _ alpha) (VarT _ beta)
    | isSafe alpha safe = markSafe beta
    | isSafe beta  safe = markSafe alpha
    | otherwise         = mempty  -- TODO(jaspervdj): Mark as unknown.

ovUnify arities safe (VarT _ alpha) y = markSafe alpha <> ovTerm arities safe y
ovUnify arities safe x (VarT _ beta)  = markSafe beta <> ovTerm arities safe x

ovUnify arities safe x y =
    ovTerm arities safe x <> ovTerm arities safe y


{-

-- NOTE(jaspervdj): This is where we would have a spot to push down and
-- propagate up types as well.
data Dir v = Down v | Up v deriving (Functor, Show)

down :: Dir ()
down = Down ()

up :: Dir ()
up = Up ()

-- | Input and output vars of an expression.
data Vars v a = Vars
    { _varsIn  :: HMS.HashMap v (NonEmpty.NonEmpty a)
    , _varsOut :: HMS.HashMap v ()
    } deriving (Eq, Show)

$(makeLenses ''Vars)

instance (Eq v, Hashable v) => Semigroup (Vars v a) where
    v1 <> v2 =
        let in0 = HMS.unionWith (<>) (v1 ^. varsIn) (v2 ^. varsIn)
            out = HMS.union (v1 ^. varsOut) (v2 ^. varsOut)
            in1 = HMS.difference in0 out in
        Vars in1 out

instance (Eq v, Hashable v) => Monoid (Vars v a) where
    mempty  = Vars mempty mempty
    mappend = (<>)

vin :: (Eq v, Hashable v) => v -> a -> Vars v a
vin v a = Vars (HMS.singleton v (NonEmpty.singleton a)) mempty

vout :: (Eq v, Hashable v) => v -> Vars v a
vout v = Vars mempty (HMS.singleton v ())

close :: (Eq v, Hashable v) => Vars v a -> Vars v a
close = varsOut .~ mempty

--------------------------------------------------------------------------------

-- | Set of declared variables.
data Scope v = Scope
    { _locals   :: !(HS.HashSet v)
    , _globals  :: !(HS.HashSet [v])  -- Maybe a function instead?
    , _builtins :: !(HMS.HashMap Function Builtins.Builtin)
    }

$(makeLenses ''Scope)

emptyScope :: Scope v
emptyScope = Scope HS.empty HS.empty HMS.empty

--------------------------------------------------------------------------------

defined :: (Eq v, Hashable v) => v -> Scope v -> Bool
defined v s = HS.member v (s ^. locals) || HS.member [v] (s ^. globals)

freeRuleBody :: Scope Var -> RuleBody a -> Vars Var a
freeRuleBody scope literals = foldMap (freeLiteral scope) literals

freeLiteral :: Scope Var -> Literal a -> Vars Var a
freeLiteral scope lit =
    freeStatement
        scope
        (if lit ^. literalNegation then down else up)
        (lit ^. literalStatement) <>
    foldMapOf (literalWith . traverse . withAs) (freeTerm scope down) lit

freeStatement :: Scope Var -> Dir ty -> Statement a -> Vars Var a
freeStatement scope _dir     (TermS t)       = freeTerm scope down t
freeStatement scope _dir     (AssignS _ v t) = vout v <> freeTerm scope down t
freeStatement scope (Up _)   (UnifyS _ x y)  = freeUnify scope x y
freeStatement scope (Down _) (UnifyS _ x y)  =
    freeTerm scope down x <> freeTerm scope down y

freeUnify :: Scope Var -> Term a -> Term a -> Vars Var a
freeUnify scope (VarT ax alpha) (VarT bx beta) =
    freeTerm scope down (VarT ax alpha) <> freeTerm scope down (VarT bx beta)
freeUnify scope (VarT _ alpha) y = vout alpha <> freeTerm scope down y
freeUnify scope x (VarT _ beta) = vout beta <> freeTerm scope down x
freeUnify scope x y = freeTerm scope down x <> freeTerm scope down y

freeTerm :: Scope Var -> Dir ty -> Term a -> Vars Var a
freeTerm scope _dir (RefT _ x k) =
    freeTerm scope down x <>
    -- The `k` goes up, since we can have things like `list[i]` where `i` is
    -- generated.
    freeTerm scope up k

freeTerm scope _ (CallT _ function args)
    | Just builtin <- HMS.lookup function Builtins.builtins =
        let dirs = replicate (Builtins.arity builtin) down ++ repeat up in
        foldMap (\(d, arg) -> freeTerm scope d arg) (zip dirs args)

    | otherwise = error "TODO(jaspervdj): user-defined functions"

freeTerm scope dir (VarT source v)
    | defined v scope = mempty
    | otherwise       = case dir of Up _ -> vout v; Down _ -> vin v source

freeTerm _ _ (ScalarT _ _) = mempty

freeTerm scope dir (ArrayT _ xs) =
    foldMap (freeTerm scope dir) xs

freeTerm scope dir (SetT _ xs) =
    foldMap (freeTerm scope dir) xs

freeTerm scope dir (ObjectT _ xs) =
    freeObject scope dir xs

freeTerm scope _dir (ArrayCompT _ chead cbody) = close $
    freeTerm scope down chead <>
    freeRuleBody scope cbody

freeTerm scope _dir (SetCompT _ chead cbody) = close $
    freeTerm scope down chead <>
    freeRuleBody scope cbody

freeTerm scope _dir (ObjectCompT _ khead vhead cbody) = close $
    freeTerm scope down khead <>
    freeTerm scope down vhead <>
    freeRuleBody scope cbody

freeObject :: Scope Var -> Dir ty -> Object a -> Vars Var a
freeObject scope dir object = foldMap
    (\(k, t) -> freeTerm scope dir k <> freeTerm scope dir t)
    object

-}
