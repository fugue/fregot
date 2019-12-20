{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Fregot.Eval.Internal
    ( Mu'

    , Context (..), unification, locals, nextInstVar, emptyContext

    , Row (..), rowContext, rowValue
    , Document

    , EvalCache

    , Environment (..), builtins, rules, inputDoc, cache, stack
    ) where

import           Control.Lens.TH           (makeLenses)
import qualified Data.HashMap.Strict       as HMS
import           Data.Unification          (Unification)
import qualified Data.Unification          as Unification
import qualified Data.Unique               as Unique
import qualified Fregot.Error.Stack        as Stack
import           Fregot.Eval.Builtins      (ReadyBuiltin)
import           Fregot.Eval.Cache         (Cache)
import           Fregot.Eval.Mu            (Mu)
import           Fregot.Eval.Value         (InstVar, Value)
import           Fregot.Names              (PackageName, Var)
import           Fregot.Prepare.Ast        (Function, Rule)
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)
import qualified Fregot.Tree               as Tree
import           Fregot.Types.Rule         (RuleType)

type Mu' = Mu Environment

data Context = Context
    { _unification :: !(Unification InstVar Mu')
    , _locals      :: !(HMS.HashMap Var InstVar)
    , _nextInstVar :: !Unique.Unique
    }

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

instance PP.Pretty PP.Sem a => PP.Pretty PP.Sem (Row a) where
    pretty (Row _ v) = PP.pretty v

type Document a = [Row a]

type EvalCache = Cache (PackageName, Var) Value

--------------------------------------------------------------------------------

data Environment = Environment
    { _builtins :: !(HMS.HashMap Function ReadyBuiltin)
    , _rules    :: !(Tree.Tree (Rule RuleType SourceSpan))
    , _inputDoc :: !Value
    , _cache    :: !EvalCache
    , _stack    :: !Stack.StackTrace
    }

$(makeLenses ''Context)
$(makeLenses ''Row)
$(makeLenses ''Environment)
