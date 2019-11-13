-- | A module that allows us to build an object in an interleaved way, and check
-- for consistencies.  This is used for sets as well as objects.
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Eval.TempObject
    ( TempObject
    , new
    , write
    , read
    ) where

import           Control.Monad.Trans       (liftIO)
import qualified Data.HashMap.Strict       as HMS
import qualified Data.IORef                as IORef
import           Fregot.Eval.Monad
import           Fregot.Eval.Value         (Value)
import           Fregot.PrettyPrint        ((<$$>))
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Prelude                   hiding (read)

type TempObject = IORef.IORef (HMS.HashMap Value Value)

new :: HMS.HashMap Value Value -> EvalM TempObject
new = liftIO . IORef.newIORef

data WriteResult = Ok | Duplicate | Inconsistent Value

-- | Write a key/value into the temporary object.  Returns if the write
-- succeeded (i.e. it was not already there).  May raise an exception if
-- there is already a different value for this key.
write :: TempObject -> SourceSpan -> Value -> Value -> EvalM Bool
write ref source k v = do
    result <- liftIO $ IORef.atomicModifyIORef' ref $ \obj ->
        case HMS.lookup k obj of
            Nothing           -> (HMS.insert k v obj, Ok)
            Just v' | v == v' -> (obj, Duplicate)
            Just v'           -> (obj, Inconsistent v')
    case result of
        Ok              -> pure True
        Duplicate       -> pure False
        Inconsistent v' -> raise' source "inconsistent object" $
            "Object key-value pairs must be consistent, but got:" <$$>
            PP.ind (PP.pretty v) <$$>
            "And:" <$$>
            PP.ind (PP.pretty v') <$$>
            "For key:" <$$>
            PP.ind (PP.pretty k)

read :: TempObject -> EvalM (HMS.HashMap Value Value)
read = liftIO . IORef.readIORef
