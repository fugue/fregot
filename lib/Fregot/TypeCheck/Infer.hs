{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.TypeCheck.Infer
    (
    ) where

import           Control.Monad             (void)
import           Control.Monad.Parachute   (ParachuteT, fatal)
import           Control.Monad.Reader      (Reader)
import           Fregot.Error              (Error)
import qualified Fregot.Error              as Error
import           Fregot.Prepare.Ast
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Fregot.TypeCheck.Types    (Type)
import qualified Fregot.TypeCheck.Types    as Types

data Ground a
    = Ground a
    | Unground  -- Should we return the variables that are not ground?
    deriving (Functor, Show)

type InferM a = ParachuteT Error (Reader ()) a

-- | Check that a future value is grounded, or throw an error.
grounded :: SourceSpan -> Ground a -> InferM a
grounded _      (Ground x) = return x
grounded source Unground   = fatal $ Error.mkError
    "typecheck" source "not grounded"
    "This expression contains free variables"

inferStatement
    :: Statement SourceSpan -> InferM ()
inferStatement = \case
    TermS t -> void $ inferTerm t
    AssignS source v t -> do
        tty <- grounded source =<< inferTerm t
        error "TODO(jaspervdj): bind type in env"
    UnifyS source l r -> inferUnify source l r

inferUnify
    :: SourceSpan -> Term SourceSpan -> Term SourceSpan -> InferM ()
inferUnify = undefined

inferTerm
    :: Term SourceSpan -> InferM (Ground Type)

inferTerm (ScalarT _ scalar) =
    return $ Ground $ inferScalar scalar

inferScalar :: Scalar -> Type
inferScalar (String _) = Types.String
