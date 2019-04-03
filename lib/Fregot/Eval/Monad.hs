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

    , EvalCache

    , Environment (..), packages, package, inputDoc, imports
    , cache, cacheVersion

    , EvalException (..)

    , EvalM
    , runEvalM

    , branch
    , unbranch
    , cut

    , negation
    , orElse
    , orElses
    , withDefault
    , requireComplete

    , toInstVar
    , lookupRule
    , clearLocals
    , withImports

    , lookupPackage
    , withPackage

    , raise
    , raise'
    ) where

import           Control.Exception         (Exception, catch, throwIO)
import           Control.Lens              (view, (&), (.~), (^.))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad.Extended    (forM)
import           Control.Monad.Reader      (MonadReader (..), ask)
import           Control.Monad.State       (MonadState (..), modify)
import           Control.Monad.Trans       (MonadIO (..))
import qualified Data.HashMap.Strict       as HMS
import           Data.List                 (find)
import           Data.Unification          (Unification)
import qualified Data.Unification          as Unification
import           Fregot.Compile.Package    (CompiledPackage)
import qualified Fregot.Compile.Package    as Package
import           Fregot.Error              (Error)
import qualified Fregot.Error              as Error
import           Fregot.Eval.Cache         (Cache)
import qualified Fregot.Eval.Cache         as Cache
import           Fregot.Eval.Value
import           Fregot.Prepare.Ast
import           Fregot.PrettyPrint        ((<$$>))
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)

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

type EvalCache = Cache (PackageName, Var) (Maybe Value, Value)

--------------------------------------------------------------------------------

data Environment = Environment
    { _packages     :: !(HMS.HashMap PackageName CompiledPackage)
    , _package      :: !CompiledPackage
    , _inputDoc     :: !Value
    , _imports      :: !(Imports SourceSpan)
    , _cache        :: !EvalCache
    , _cacheVersion :: !Cache.Version
    }

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
    {-# INLINE pure #-}

    EvalM mf <*> EvalM mx = EvalM $ \rs ctx0 -> do
        rows1 <- mf rs ctx0
        fmap concat $ forM rows1 $ \row1 -> do
            rows2 <- mx rs (row1 ^. rowContext)
            forM rows2 $ \row2 -> return $!
                row2 & rowValue .~ (row1 ^. rowValue) (row2 ^. rowValue)
    {-# INLINE (<*>) #-}

instance Monad EvalM where
    EvalM mx >>= f = EvalM $ \rs ctx0 -> do
        xrows <- mx rs ctx0
        fmap concat $ forM xrows $ \(Row ctx1 x) ->
            unBranchM (f x) rs ctx1
    {-# INLINE (>>=) #-}

instance MonadReader Environment EvalM where
    ask = EvalM $ \rs ctx -> return [Row ctx rs]
    {-# INLINE ask #-}

    local l (EvalM f) = EvalM $ \rs ctx -> f (l rs) ctx
    {-# INLINE local #-}

instance MonadState Context EvalM where
    get = EvalM $ \_ ctx  -> return [Row ctx ctx]
    {-# INLINE get #-}

    put ctx = EvalM $ \_ _    -> return [Row ctx ()]
    {-# INLINE put #-}

    state f = EvalM $ \_ ctx0 -> let (x, ctx1) = f ctx0 in return [Row ctx1 x]
    {-# INLINE state #-}

instance MonadIO EvalM where
    liftIO mio = EvalM $ \_ ctx -> mio >>= \x -> return [Row ctx x]

runEvalM :: Environment -> EvalM a -> IO (Either Error (Document a))
runEvalM rules0 (EvalM f) = catch
    (Right <$> f rules0 emptyContext)
    (\(EvalException err) -> return (Left err))

branch :: [EvalM a] -> EvalM a
branch options = EvalM $ \rs ctx ->
    fmap concat $ forM options $ \(EvalM opt) -> opt rs ctx
{-# INLINE branch #-}

unbranch :: EvalM a -> EvalM [a]
unbranch (EvalM f) = EvalM $ \rs ctx -> do
    rows <- f rs ctx
    return [Row ctx (map (view rowValue) rows)]
{-# INLINE unbranch #-}

cut :: EvalM a
cut = EvalM $ \_ _ -> return []
{-# INLINE cut #-}

negation :: (a -> Bool) -> EvalM a -> EvalM ()
negation trueish (EvalM f) = EvalM $ \rs ctx -> do
    rows <- filter (\(Row _ x) -> trueish x) <$> (f rs ctx)
    return $! if null rows then [Row ctx ()] else []

orElse :: EvalM a -> EvalM a -> EvalM a
orElse (EvalM x) (EvalM alt) = EvalM $ \env ctx -> do
    rows <- x env ctx
    case rows of
        [] -> alt env ctx
        xs -> return xs

orElses :: EvalM a -> [EvalM a] -> EvalM a
orElses x []           = x
orElses x (alt : alts) = x `orElse` orElses alt alts

withDefault :: EvalM a -> EvalM a -> EvalM a
withDefault = flip orElse

requireComplete
    :: (Eq a, PP.Pretty PP.Sem a) => SourceSpan -> EvalM a -> EvalM a
requireComplete source (EvalM f) = EvalM $ \env ctx -> do
    rows <- f env ctx
    case rows of
        (r : more)
            | Just d <- find ((/= r ^. rowValue) . view rowValue) more ->
                throwIO $ EvalException $ Error.mkError
                    "eval" source "inconsistent result" $
                    "Inconsistent result for complete rule, but got:" <$$>
                    PP.ind (PP.pretty $ r ^. rowValue) <$$>
                    "And:" <$$>
                    PP.ind (PP.pretty $ d ^. rowValue)
            | otherwise -> return [r]
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
lookupRule vs = fail $ "todo: lookup rules in other packages: " ++ show vs

clearLocals :: EvalM a -> EvalM a
clearLocals mx = do
    oldLocals <- state $ \ctx -> (_locals ctx, ctx {_locals = mempty})
    x         <- mx
    modify $ \ctx -> ctx {_locals = oldLocals}
    return x

withImports :: Imports SourceSpan -> EvalM a -> EvalM a
withImports imps = local (imports .~ imps)

lookupPackage :: PackageName -> EvalM (Maybe CompiledPackage)
lookupPackage pkgname = do
    pkgs <- view packages
    return $! HMS.lookup pkgname pkgs

withPackage :: CompiledPackage -> EvalM a -> EvalM a
withPackage pkg = local (package .~ pkg)

-- | Raise an error.  We currently don't allow catching exceptions, but they are
-- handled at the top level `runEvalM` and converted to an `Either`.
raise :: Error -> EvalM a
raise err = EvalM (\_ _ -> throwIO (EvalException err))

raise' :: SourceSpan -> PP.SemDoc -> PP.SemDoc -> EvalM a
raise' source title body = raise (Error.mkError "eval" source title body)
