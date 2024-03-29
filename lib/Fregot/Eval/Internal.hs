{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Fregot.Eval.Internal
    ( Mu'

    , Context (..), unification, locals, nextInstVar, emptyContext

    , Row (..), rowContext, rowValue
    , Document
    , prettyRowWithContext

    , RuleCache
    , ComprehensionCache

    , Environment (..), builtins, rules, inputDoc, ruleCache, comprehensionCache
    , stack, strictBuiltinErrors
    ) where

import           Control.Lens.TH           (makeLenses)
import qualified Data.HashMap.Strict       as HMS
import           Data.List                 (sortOn)
import           Data.Maybe                (maybeToList)
import           Data.Unification          (Unification)
import qualified Data.Unification          as Unification
import           Data.Unique               (Unique)
import qualified Data.Unique               as Unique
import           Fregot.Builtins.Internal  (ReadyBuiltin)
import           Fregot.Compile.Internal   (CompiledRule)
import qualified Fregot.Error.Stack        as Stack
import           Fregot.Eval.Cache         (Cache)
import           Fregot.Eval.Mu            (Mu)
import           Fregot.Eval.Value         (InstVar, Value)
import           Fregot.Names              (PackageName, Var)
import           Fregot.Prepare.Ast        (Function)
import           Fregot.PrettyPrint        ((<+>))
import qualified Fregot.PrettyPrint        as PP
import qualified Fregot.Tree               as Tree

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

type RuleCache = Cache (PackageName, Var) Value

type ComprehensionCache = Cache Unique (HMS.HashMap [Value] Mu')

prettyRowWithContext:: PP.Pretty PP.Sem a => Row a -> PP.SemDoc
prettyRowWithContext (Row context value) = PP.vcat $
    [PP.punctuation "=" <+> PP.pretty value] ++
    [ PP.punctuation "|" <+> PP.pretty var <+> PP.punctuation "=" <+>
        PP.nest 2 (PP.pretty mu)
    | (var, iv) <- sortOn fst . HMS.toList $ _locals context
    , mu <- maybeToList . Unification.lookupMaybe iv $ _unification context
    ]

--------------------------------------------------------------------------------

data Environment = Environment
    { _builtins            :: !(HMS.HashMap Function ReadyBuiltin)
    , _rules               :: !(Tree.Tree CompiledRule)
    , _inputDoc            :: !Value
    , _ruleCache           :: !RuleCache
    , _comprehensionCache  :: !ComprehensionCache
    , _stack               :: !Stack.StackTrace
    , _strictBuiltinErrors :: !Bool
    }

$(makeLenses ''Context)
$(makeLenses ''Row)
$(makeLenses ''Environment)
