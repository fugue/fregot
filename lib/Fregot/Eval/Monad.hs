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

    , Environment (..), builtins, rules, inputDoc
    , cache, stack

    , EvalException (..)

    , EnvContext (..), ecEnvironment, ecContext
    , EvalM
    , runEvalM

    , ResumeStep
    , newResumeStep
    , Step (..)
    , runStep

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

    , pushStackFrame
    , pushRuleStackFrame
    , pushFunctionStackFrame

    , raise
    , raise'

    , runBuiltinM
    ) where

import           Control.Lens              (view, (%~), (&), (.~), (^.))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad.Reader      (MonadReader (..), ask)
import           Control.Monad.State       (MonadState (..), modify)
import           Control.Monad.Stream      (Stream)
import qualified Control.Monad.Stream      as Stream
import           Control.Monad.Trans       (MonadIO (..))
import qualified Data.HashMap.Strict       as HMS
import           Data.List                 (find)
import           Fregot.Error              (Error)
import qualified Fregot.Error              as Error
import qualified Fregot.Error.Stack        as Stack
import           Fregot.Eval.Builtins     (BuiltinException (..))
import           Fregot.Eval.Internal
import           Fregot.Eval.Value
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.PrettyPrint        ((<$$>))
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)
import qualified Fregot.Tree               as Tree
import           Fregot.Types.Rule         (RuleType)

data EvalException = EvalException Environment Context Error

instance Show EvalException where
    -- This show horrible is useless, in practice we'll catch the error and
    -- pretty-print the thing inside.
    show _ = "EvalException _ _ _"

type Suspension = (SourceSpan, Context, Environment)

newtype EvalM a = EvalM
    { unEvalM
        :: Environment -> Context -> Stream EvalException Suspension IO (Row a)
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

-- | Everything you need to evaluate something in a context.
data EnvContext = EnvContext
    { _ecContext     :: !Context
    , _ecEnvironment :: !Environment
    }

$(makeLenses ''EnvContext)

runEvalM :: EnvContext -> EvalM a -> IO (Either Error (Document a))
runEvalM (EnvContext ctx0 env0) (EvalM f) =
    Stream.toList $
    Stream.mapError (\(EvalException _env _context err) -> err) $
    f env0 ctx0

type ResumeStep a = (EnvContext, Stream EvalException Suspension IO (Row a))

newResumeStep :: EnvContext -> EvalM a -> ResumeStep a
newResumeStep envctx@(EnvContext ctx env) (EvalM f) = (envctx, f env ctx)

data Step a
    = Yield   (Row a)    EnvContext (Stream EvalException Suspension IO (Row a))
    | Suspend SourceSpan EnvContext (Stream EvalException Suspension IO (Row a))
    | Error              EnvContext Error
    | Done

runStep :: ResumeStep a -> IO (Step a)
runStep (ss, stream) = do
    sstep <- Stream.step stream
    case sstep of
        Stream.Yield r ns ->
            return $ Yield r (ss & ecContext .~ (r ^. rowContext)) ns
        Stream.Suspend (i, ctx, env') ns ->
            return $ Suspend i (EnvContext ctx env') ns
        Stream.Done         -> return Done
        Stream.Error e      ->
            let (EvalException env context err) = e in
            return (Error (EnvContext context env) err)

suspend :: SourceSpan -> EvalM a -> EvalM a
suspend source (EvalM f) =
    EvalM $ \env ctx -> Stream.suspend (source, ctx, env) (f env ctx)
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
negation isTrue (EvalM f) = EvalM $ \rs ctx -> do
    peek <- Stream.peek $ Stream.filter (isTrue . view rowValue) (f rs ctx)
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
                Stream.throw $ EvalException env (r ^. rowContext) $ Error.mkError
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

lookupRule :: Name -> EvalM (Maybe (Rule RuleType SourceSpan))
lookupRule (LocalName _) = pure Nothing
lookupRule (QualifiedName key) = do
    env0 <- ask
    pure $ Tree.lookup key (env0 ^. rules)
lookupRule _ = pure Nothing

clearLocals :: EvalM a -> EvalM a
clearLocals mx = do
    oldLocals <- state $ \ctx -> (_locals ctx, ctx {_locals = mempty})
    x         <- mx
    modify $ \ctx -> ctx {_locals = oldLocals}
    return x

pushStackFrame :: Stack.StackFrame -> EvalM a -> EvalM a
pushStackFrame frame = local (stack %~ Stack.push frame)

pushRuleStackFrame :: SourceSpan -> Name -> EvalM a -> EvalM a
pushRuleStackFrame source n = local $ \env -> env
    { _stack = Stack.push (Stack.RuleStackFrame n source) (env ^. stack)
    }

pushFunctionStackFrame :: SourceSpan -> Name -> EvalM a -> EvalM a
pushFunctionStackFrame src n = local $ \env -> env
    { _stack = Stack.push (Stack.FunctionStackFrame n src) (env ^. stack)
    }

-- | Raise an error.  We currently don't allow catching exceptions, but they are
-- handled at the top level `runEvalM` and converted to an `Either`.
raise :: Error -> EvalM a
raise err = EvalM $ \env ctx ->
    Stream.throw $ EvalException env ctx $
    err & Error.stack .~ (env ^. stack)

raise' :: SourceSpan -> PP.SemDoc -> PP.SemDoc -> EvalM a
raise' source title body = raise (Error.mkError "eval" source title body)

runBuiltinM :: SourceSpan -> Stream BuiltinException Suspension IO a -> EvalM a
runBuiltinM source stream = EvalM $ \env ctx ->
    -- Stream.throw $ EvalException env ctx $
    Stream.mapError
        (\(BuiltinException err) -> EvalException env ctx $
            (Error.mkError "eval" source "builtin error" $ PP.pretty err)
                & Error.stack .~ (env ^. stack))
        (Row ctx <$> stream)
