{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Fregot.Eval.Monad
    ( Context (..), unification, locals, nextInstVar
    , emptyContext

    , Row (..), rowContext, rowValue
    , Document

    , Environment (..), packages, package, inputDoc, imports

    , EvalException (..)

    , EvalM
    , runEvalM

    , branch
    , unbranch
    , cut

    , negation
    , withDefault
    , requireComplete

    , toInstVar
    , lookupRule
    , clearLocals
    , withImports
    , withPackage

    , raise
    , raise'
    ) where

import           Control.Exception          (Exception, catch, throwIO)
import           Control.Lens               (view, (&), (.~), (^.))
import           Control.Lens.TH            (makeLenses)
import           Control.Monad.Extended     (forM)
import           Control.Monad.Reader       (MonadReader (..), ask)
import           Control.Monad.State        (MonadState (..), modify)
import qualified Data.HashMap.Strict        as HMS
import           Data.Unification           (Unification)
import qualified Data.Unification           as Unification
import           Fregot.Error               (Error)
import qualified Fregot.Error               as Error
import           Fregot.Eval.Value
import           Fregot.Interpreter.Package (Package)
import qualified Fregot.Interpreter.Package as Package
import           Fregot.Prepare.AST
import qualified Fregot.PrettyPrint         as PP
import           Fregot.Sources.SourceSpan  (SourceSpan)

data Context = Context
    { _unification :: !(Unification InstVar Value)
    , _locals      :: !(HMS.HashMap Var InstVar)
    , _nextInstVar :: !Int
    }

$(makeLenses ''Context)

emptyContext :: Context
emptyContext = Context
    { _unification = Unification.empty
    , _locals      = mempty
    , _nextInstVar = 0
    }

data Row a = Row
    { _rowContext :: !Context
    , _rowValue   :: !a
    } deriving (Functor)

$(makeLenses ''Row)

instance PP.Pretty PP.Sem a => PP.Pretty PP.Sem (Row a) where
    pretty (Row _ v) = PP.pretty v

type Document a = [Row a]

--------------------------------------------------------------------------------

data Environment = Environment
    { _packages :: !(HMS.HashMap PackageName Package)

    -- NOTE(jaspervdj): We'll need to update package as well if call a rule from
    -- another package.
    , _package  :: !Package
    , _inputDoc :: !Value
    , _imports  :: !(Imports SourceSpan)
    } deriving (Show)

$(makeLenses ''Environment)

newtype EvalException = EvalException Error

instance Show EvalException where
    -- This show horrible is useless, in practice we'll catch the error and
    -- pretty-print the thing inside.
    show _ = "EvalException _"

instance Exception EvalException

newtype EvalM a = EvalM {unBranchM :: Environment -> Context -> IO [Row a]}
    deriving (Functor)

instance Applicative EvalM where
    pure x = EvalM $ \_ ctx -> return [Row ctx x]
    EvalM mf <*> EvalM mx = EvalM $ \rs ctx0 -> do
        rows1 <- mf rs ctx0
        fmap concat $ forM rows1 $ \row1 -> do
            rows2 <- mx rs (row1 ^. rowContext)
            forM rows2 $ \row2 -> return $!
                row2 & rowValue .~ (row1 ^. rowValue) (row2 ^. rowValue)

instance Monad EvalM where
    EvalM mx >>= f = EvalM $ \rs ctx0 -> do
        xrows <- mx rs ctx0
        fmap concat $ forM xrows $ \(Row ctx1 x) ->
            unBranchM (f x) rs ctx1

instance MonadReader Environment EvalM where
    ask = EvalM $ \rs ctx -> return [Row ctx rs]
    local l (EvalM f) = EvalM $ \rs ctx -> f (l rs) ctx

instance MonadState Context EvalM where
    get     = EvalM $ \_ ctx  -> return [Row ctx ctx]
    put ctx = EvalM $ \_ _    -> return [Row ctx ()]
    state f = EvalM $ \_ ctx0 -> let (x, ctx1) = f ctx0 in return [Row ctx1 x]

runEvalM :: Environment -> EvalM a -> IO (Either Error (Document a))
runEvalM rules0 (EvalM f) = catch
    (Right <$> f rules0 emptyContext)
    (\(EvalException err) -> return (Left err))

branch :: [EvalM a] -> EvalM a
branch options = EvalM $ \rs ctx ->
    fmap concat $ forM options $ \(EvalM opt) -> opt rs ctx

unbranch :: EvalM a -> EvalM [a]
unbranch (EvalM f) = EvalM $ \rs ctx -> do
    rows <- f rs ctx
    return [Row ctx (map (view rowValue) rows)]

cut :: EvalM a
cut = EvalM $ \_ _ -> return []

negation :: (a -> Bool) -> EvalM a -> EvalM ()
negation trueish (EvalM f) = EvalM $ \rs ctx -> do
    rows <- filter (\(Row _ x) -> trueish x) <$> (f rs ctx)
    return $! if null rows then [Row ctx ()] else []

withDefault :: EvalM a -> EvalM a -> EvalM a
withDefault (EvalM def) (EvalM f) = EvalM $ \env ctx -> do
    rows <- f env ctx
    case rows of
        [] -> def env emptyContext
        xs -> return xs

requireComplete :: Eq a => EvalM a -> EvalM a
requireComplete (EvalM f) = EvalM $ \env ctx -> do
    rows <- f env ctx
    case rows of
        (r : more)
            | all ((== r ^. rowValue) . view rowValue) more -> return [r]
            | otherwise -> fail
                -- TODO(jaspervdj): Better error message.
                "requireComplete: inconsistent result for complete rule"
        _          -> return rows

-- | Turn a variable into an instantiated variable.
--
-- * If it's a local variable and already instantiated, we return that.
-- * Otherwise, we instantiate a new one.
toInstVar :: Var -> EvalM InstVar
toInstVar v = state $ \ctx -> case HMS.lookup v (ctx ^. locals) of
    Just iv -> (iv, ctx)
    Nothing ->
        let !iv   = InstVar (ctx ^. nextInstVar) v
            !lcls = HMS.insert v iv (ctx ^. locals) in
        (iv, ctx {_nextInstVar = _nextInstVar ctx + 1, _locals = lcls})

lookupRule :: [Var] -> EvalM (Maybe (Rule SourceSpan))
lookupRule [root] = do
    env0 <- ask
    return $ Package.lookup root (env0 ^. package)
lookupRule _ = fail "todo: lookup rules in other packages"

clearLocals :: EvalM a -> EvalM a
clearLocals mx = do
    oldLocals <- state $ \ctx -> (_locals ctx, ctx {_locals = mempty})
    x         <- mx
    modify $ \ctx -> ctx {_locals = oldLocals}
    return x

withImports :: Imports SourceSpan -> EvalM a -> EvalM a
withImports imps = local (imports .~ imps)

withPackage :: PackageName -> EvalM a -> EvalM a
withPackage pkgname mx = do
    pkgs <- view packages
    case HMS.lookup pkgname pkgs of
        Just pkg -> local (package .~ pkg) mx
        Nothing  -> fail $
            "Unknown package: " ++ show pkgname ++ ", known packages: " ++
            show (HMS.keys pkgs)

-- | Raise an error.  We currently don't allow catching exceptions, but they are
-- handled at the top level `runEvalM` and converted to an `Either`.
raise :: Error -> EvalM a
raise err = EvalM (\_ _ -> throwIO (EvalException err))

raise' :: SourceSpan -> PP.SemDoc -> PP.SemDoc -> EvalM a
raise' source title body = raise (Error.mkError "eval" source title body)
