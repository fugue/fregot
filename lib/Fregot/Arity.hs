module Fregot.Arity
    ( ArityCheck (..)
    , checkArity
    ) where

data ArityCheck a
    = ArityOk  [a] (Maybe a)
    | ArityBad Int

checkArity :: Int -> [a] -> ArityCheck a
checkArity arity args
    | ngiven < arity     = ArityBad ngiven
    | null remainder     = ArityOk given Nothing
    | [out] <- remainder = ArityOk given (Just out)
    | otherwise          = ArityBad (ngiven + length remainder)
  where
    ngiven             = length given
    (given, remainder) = splitAt arity args
