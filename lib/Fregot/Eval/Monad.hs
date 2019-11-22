-- | Monad and utilities for query evaluation.
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Fregot.Eval.Monad where

import           Control.Lens              (view, (%~), (&), (.~), (^.))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad.Reader      (MonadReader (..))
import           Control.Monad.State       (MonadState (..))
import           Data.Bifunctor            (first)
import qualified Data.HashMap.Strict       as HMS
import           Data.List                 (find)
import           Data.Unification          (Unification)
import qualified Data.Unification          as Unification
import qualified Data.Unique               as Unique
import           Fregot.Error              (Error)
import qualified Fregot.Error              as Error
import qualified Fregot.Error.Stack        as Stack
import           Fregot.Eval.Bistream      (Bistream)
import qualified Fregot.Eval.Bistream      as Bistream
import           Fregot.Eval.Builtins      (ReadyBuiltin)
import           Fregot.Eval.Tree
import           Fregot.Eval.Value
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.PrettyPrint        ((<$$>))
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)

-- | Context is the state for query evaluation; i.e. parts that change from one
-- literal to the next.
data Context = Context
    { _unification :: !(Unification InstVar Value)
    , _locals      :: !(HMS.HashMap Var InstVar)
    , _nextInstVar :: !Unique.Unique
    }

emptyContext :: Context
emptyContext = Context
    { _unification = Unification.empty
    , _locals      = mempty
    , _nextInstVar = 0
    }

-- | A row is a single result from a query.  This consists of the actual value,
-- and the corresponding 'Context', since the latter may be different for every
-- row.
data Row a = Row
    { _rowContext :: !Context
    , _rowValue   :: !a
    } deriving (Functor)

instance PP.Pretty PP.Sem a => PP.Pretty PP.Sem (Row a) where
    pretty (Row _ v) = PP.pretty v

type Document a = [Row a]

data Environment = Environment
    { _builtins :: !(HMS.HashMap Function ReadyBuiltin)
    -- | Values in the data document tree are lazily computed.  Because we have
    -- the recursion check, we shouldn't get into an infite recursion here.
    , _dataDoc  :: !(Tree (Bistream EvalException Suspension Value))
    , _inputDoc :: !Value
    , _stack    :: !Stack.StackTrace
    }

data EvalException = EvalException Environment Context Error

instance Show EvalException where
    -- This show horrible is useless, in practice we'll catch the error and
    -- pretty-print the thing inside.
    show _ = "EvalException _ _ _"

type Suspension = (SourceSpan, Context, Environment)

newtype EvalM a = EvalM
    { unEvalM
        :: Environment -> Context
        -> Bistream EvalException Suspension (Row a)
    } deriving (Functor)

-- | Everything you need to evaluate something in a context.
data EnvContext = EnvContext
    { _ecContext     :: !Context
    , _ecEnvironment :: !Environment
    }

$(makeLenses ''Context)
$(makeLenses ''Row)
$(makeLenses ''Environment)
$(makeLenses ''EnvContext)

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

runEvalM :: EnvContext -> EvalM a -> Either Error (Document a)
runEvalM (EnvContext ctx0 env0) (EvalM f) =
    first (\(EvalException _env _context err) -> err) $
    Bistream.toErrorOrList (f env0 ctx0)

type ResumeStep a = (EnvContext, Bistream EvalException Suspension (Row a))

newResumeStep :: EnvContext -> EvalM a -> ResumeStep a
newResumeStep envctx@(EnvContext ctx env) (EvalM f) = (envctx, f env ctx)

data Step a
    = Yield   (Row a)    EnvContext (Bistream EvalException Suspension (Row a))
    | Suspend SourceSpan EnvContext (Bistream EvalException Suspension (Row a))
    | Error              EnvContext Error
    | Done

runStep :: ResumeStep a -> Step a
runStep (ss, stream) = case stream of
    Bistream.Done ->
        Done
    Bistream.RCons r bs ->
        Yield r (ss & ecContext .~ (r ^. rowContext)) bs
    Bistream.RSingle r ->
        Yield r (ss & ecContext .~ (r ^. rowContext)) Bistream.Done
    Bistream.LCons (i, ctx, env') bs ->
        Suspend i (EnvContext ctx env') bs
    Bistream.Error (EvalException env context err) ->
        Error (EnvContext context env) err

suspend :: SourceSpan -> EvalM a -> EvalM a
suspend source (EvalM f) =
    EvalM $ \env ctx -> Bistream.LCons (source, ctx, env) (f env ctx)
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
    Row ctx . map (view rowValue) <$> Bistream.collapse (f rs ctx)
{-# INLINE unbranch #-}

cut :: EvalM a
cut = EvalM $ \_ _ -> mempty
{-# INLINE cut #-}

negation :: (a -> Bool) -> EvalM a -> EvalM ()
negation true (EvalM f) = EvalM $ \rs ctx -> do
    isNull <- Bistream.null $ Bistream.filter (true . view rowValue) (f rs ctx)
    if isNull then pure (Row ctx ()) else mempty

orElse :: EvalM a -> EvalM a -> EvalM a
orElse (EvalM x) (EvalM alt) = EvalM $ \env ctx -> do
    let stream = x env ctx
    isNull <- Bistream.null (x env ctx)
    if isNull then alt env ctx else stream

orElses :: EvalM a -> [EvalM a] -> EvalM a
orElses x []           = x
orElses x (alt : alts) = x `orElse` orElses alt alts

withDefault :: EvalM a -> EvalM a -> EvalM a
withDefault = flip orElse

requireComplete
    :: (Eq a, PP.Pretty PP.Sem a) => SourceSpan -> EvalM a -> EvalM a
requireComplete source (EvalM f) = EvalM $ \env ctx -> do
    rows <- Bistream.collapse (f env ctx)
    case rows of
        (r : more)
            | Just d <- find ((/= r ^. rowValue) . view rowValue) more ->
                Bistream.Error $
                EvalException env (r ^. rowContext) $ Error.mkError
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
    Bistream.Error $ EvalException env ctx $
    err & Error.stack .~ (env ^. stack)

raise' :: SourceSpan -> PP.SemDoc -> PP.SemDoc -> EvalM a
raise' source title body = raise (Error.mkError "eval" source title body)
