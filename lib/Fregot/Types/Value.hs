-- | Inferring already-evaluated values.
{-# LANGUAGE LambdaCase #-}
module Fregot.Types.Value
    ( TypeContext
    , inferContext
    , inferMu
    , inferValue
    ) where

import           Control.Arrow         ((>>>))
import           Control.Lens          (review, (^.))
import           Data.Either           (partitionEithers)
import qualified Data.HashMap.Strict   as HMS
import qualified Data.HashSet          as HS
import qualified Data.Unification      as Unification
import qualified Data.Vector           as V
import qualified Fregot.Eval.Internal  as Eval
import qualified Fregot.Eval.Mu        as Mu
import qualified Fregot.Eval.Number    as Number
import qualified Fregot.Eval.Value     as V
import           Fregot.Names          (Var)
import qualified Fregot.Prepare.Ast    as Ast
import           Fregot.Types.Internal
import           Prelude               hiding (null)

type TypeContext = HMS.HashMap Var Type

inferContext :: Eval.Context -> TypeContext
inferContext ctx = HMS.mapMaybe
    (\v -> inferMu <$> Unification.lookupMaybe v (ctx ^. Eval.unification))
    (ctx ^. Eval.locals)

inferMu :: Mu.Mu -> Type
inferMu = inferMuF . fmap inferMu . Mu.unMu

inferMuF :: Mu.MuF Type -> Type
inferMuF = \case
    Mu.RecM      v -> inferValueF v
    Mu.GroundedM v -> inferValue  v
    Mu.FreeM     _ -> unknown
    Mu.WildcardM   -> unknown
    Mu.TreeM _ _   -> unknown

inferValue :: V.Value -> Type
inferValue = inferValueF . fmap inferValue . V.unValue

inferValueF :: V.ValueF Type -> Type
inferValueF = \case
    V.StringV _   -> string
    V.NumberV _   -> number
    V.BoolV _     -> boolean
    V.NullV       -> null
    V.ArrayV arr  -> arrayOf $ unions $ V.toList arr
    V.SetV set    -> setOf $ unions $ map inferValue $ HS.toList set
    V.ObjectV obj ->
        let (static, dynamic) = partitionEithers $ do
                (k, v) <- HMS.toList obj
                pure $ case valueToScalar k of
                    Just s  -> Left (s, v)
                    Nothing -> Right (inferValue k, v) in
        review singleton $ Object $ Obj
            (HMS.fromList static)
            (case dynamic of
                []    -> Nothing
                _ : _ -> Just (unions (map fst dynamic), unions (map snd dynamic)))

valueToScalar :: V.Value -> Maybe Ast.Scalar
valueToScalar = V.unValue >>> \case
    V.NullV     -> Just Ast.Null
    V.BoolV b   -> Just $ Ast.Bool b
    V.StringV s -> Just $ Ast.String s
    V.NumberV n -> Just $ Ast.Number $ Number.toScientific n
    _           -> Nothing
