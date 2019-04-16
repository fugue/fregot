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
    , cache, cacheVersion, stack

    , EvalException (..)

    , EvalM
    , runEvalM

    , StepState (..), ssEnvironment, ssContext
    , mkStepState
    , Step (..)
    , stepEvalM

    , suspend
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
import           Control.Monad.Reader      (MonadReader (..), ask)
import           Control.Monad.State       (MonadState (..), modify)
import           Control.Monad.Trans       (MonadIO (..))
import           Control.Monad.Stream      (Stream)
import qualified Control.Monad.Stream      as Stream
import qualified Data.HashMap.Strict       as HMS
import           Data.List                 (find)
import           Data.Unification          (Unification)
import qualified Data.Unification          as Unification
import qualified Data.Unique               as Unique
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
    , _nextInstVar :: !Unique.Unique
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

type EvalCache = Cache (PackageName, Var) Value

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

type Suspension = (SourceSpan, Context)

newtype EvalM a = EvalM
    { unEvalM :: Environment -> Context -> Stream Suspension IO (Row a)
    } deriving (Functor)

instance Applicative EvalM where
    pure x = EvalM $ \_ ctx -> return (Row ctx x)
    {-# INLINE pure #-}

    EvalM mf <*> EvalM mx = EvalM $ \rs ctx0 -> do
        row1 <- mf rs ctx0
        row2 <- mx rs (row1 ^. rowContext)
        return $! row2 & rowValue .~ (row1 ^. rowValue) (row2 ^. rowValue)
    {-# INLINE (<*>) #-}

instance Monad EvalM where
    EvalM mx >>= f = EvalM $ \rs ctx0 -> do
        Row ctx1 x <- mx rs ctx0
        unEvalM (f x) rs ctx1
    {-# INLINE (>>=) #-}

instance MonadReader Environment EvalM where
    ask = EvalM $ \rs ctx -> return (Row ctx rs)
    {-# INLINE ask #-}

    local l (EvalM f) = EvalM $ \rs ctx -> f (l rs) ctx
    {-# INLINE local #-}

instance MonadState Context EvalM where
    get = EvalM $ \_ ctx  -> return (Row ctx ctx)
    {-# INLINE get #-}

    put ctx = EvalM $ \_ _ -> return (Row ctx ())
    {-# INLINE put #-}

    state f = EvalM $ \_ ctx0 -> let (x, ctx1) = f ctx0 in return (Row ctx1 x)
    {-# INLINE state #-}

instance MonadIO EvalM where
    liftIO mio = EvalM $ \_ ctx -> fmap (Row ctx) (liftIO mio)
    {-# INLINE liftIO #-}

runEvalM :: Environment -> Context -> EvalM a -> IO (Either Error (Document a))
runEvalM env0 ctx0 (EvalM f) = catch
    (Right <$> Stream.toList (f env0 ctx0))
    (\(EvalException err) -> return (Left err))

data StepState a = StepState
    { _ssStream      :: Stream Suspension IO (Row a)
    , _ssContext     :: Context
    , _ssEnvironment :: Environment
    }

mkStepState :: Environment -> EvalM a -> StepState a
mkStepState env0 (EvalM f) = StepState
    { _ssStream      = f env0 emptyContext
    , _ssContext     = emptyContext
    , _ssEnvironment = env0
    }

data Step a
    = Yield (Row a) (StepState a)
    | Suspend SourceSpan (StepState a)
    | Done
    -- NOTE(jaspervdj): We can recover the latest 'StepState' here?
    | Error Error

$(makeLenses ''StepState)

stepEvalM :: StepState a -> IO (Step a)
stepEvalM ss = catch
    (do
        sstep <- Stream.step (ss ^. ssStream)
        let env = ss ^. ssEnvironment
        case sstep of
            Stream.Yield r ns ->
                return $ Yield r (StepState ns (r ^. rowContext) env)
            Stream.Suspend (i, ctx) ns ->
                return $ Suspend i (StepState ns ctx env)
            Stream.Done         -> return Done)
    (\(EvalException err) -> return (Error err))

suspend :: SourceSpan -> EvalM a -> EvalM a
suspend source (EvalM f) =
    EvalM $ \rs ctx -> Stream.suspend (source, ctx) (f rs ctx)
{-# INLINE suspend #-}

branch :: [EvalM a] -> EvalM a
branch [opt]   =
    -- It is very common in typical code to only have a single branch.  This
    -- case speeds things up a bit by possibly preserving the 'Single'
    -- constructor.
    opt
branch options = EvalM $ \rs ctx ->
    let go []               = mempty
        go (EvalM o : opts) = o rs ctx <> go opts in
    go options
{-# INLINE branch #-}

unbranch :: EvalM a -> EvalM [a]
unbranch (EvalM f) = EvalM $ \rs ctx ->
    -- NOTE(jaspervdj): We are effectively dropping a part of the context here
    -- which I'm not sure is safe.
    Row ctx . map (view rowValue) <$> Stream.collapse (f rs ctx)
{-# INLINE unbranch #-}

cut :: EvalM a
cut = EvalM $ \_ _ -> mempty
{-# INLINE cut #-}

negation :: (a -> Bool) -> EvalM a -> EvalM ()
negation trueish (EvalM f) = EvalM $ \rs ctx -> do
    peek <- Stream.peek $ Stream.filter (trueish . view rowValue) (f rs ctx)
    case peek of
        Nothing -> pure (Row ctx ())
        Just _  -> mempty

orElse :: EvalM a -> EvalM a -> EvalM a
orElse (EvalM x) (EvalM alt) = EvalM $ \env ctx -> do
    peek <- Stream.peek (x env ctx)
    case peek of
        Nothing -> alt env ctx
        Just x' -> x'

orElses :: EvalM a -> [EvalM a] -> EvalM a
orElses x []           = x
orElses x (alt : alts) = x `orElse` orElses alt alts

withDefault :: EvalM a -> EvalM a -> EvalM a
withDefault = flip orElse

requireComplete
    :: (Eq a, PP.Pretty PP.Sem a) => SourceSpan -> EvalM a -> EvalM a
requireComplete source (EvalM f) = EvalM $ \env ctx -> do
    rows <- Stream.collapse (f env ctx)
    case rows of
        (r : more)
            | Just d <- find ((/= r ^. rowValue) . view rowValue) more ->
                liftIO $ throwIO $ EvalException $ Error.mkError
                    "eval" source "inconsistent result" $
                    "Inconsistent result for complete rule, but got:" <$$>
                    PP.ind (PP.pretty $ r ^. rowValue) <$$>
                    "And:" <$$>
                    PP.ind (PP.pretty $ d ^. rowValue)
            | otherwise -> pure r
        _ -> mempty

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
raise err = EvalM (\_ _ -> liftIO $ throwIO (EvalException err))

raise' :: SourceSpan -> PP.SemDoc -> PP.SemDoc -> EvalM a
raise' source title body = raise (Error.mkError "eval" source title body)
