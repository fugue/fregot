{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}
module Fregot.Prepare.Vars
    ( Dir (..)
    , Vars (..), varsIn, varsOut

    , Scope (..), locals, globals, builtins
    , emptyScope

    , freeRuleBody
    , freeStatement
    , freeTerm
    ) where

import           Control.Lens                (foldMapOf, (.~), (^.))
import           Control.Lens.TH             (makeLenses)
import           Data.Hashable               (Hashable)
import qualified Data.HashMap.Strict         as HMS
import qualified Data.HashSet                as HS
import qualified Data.List.NonEmpty.Extended as NonEmpty
import qualified Fregot.Eval.Builtins        as Builtins
import           Fregot.Prepare.Ast

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
