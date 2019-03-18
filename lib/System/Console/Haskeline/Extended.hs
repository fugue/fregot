module System.Console.Haskeline.Extended
    ( module System.Console.Haskeline
    , concatCompletion
    , completeDictionary
    ) where

import           Data.List                (isPrefixOf)
import           System.Console.Haskeline

concatCompletion :: Monad m => [CompletionFunc m] -> CompletionFunc m
concatCompletion cfuncs = \inp -> do
    results <- mapM ($ inp) cfuncs
    let flattened = do
            (pre, comps) <- results
            comp <- comps
            return $ comp {replacement = reverse pre ++ replacement comp}
    return ("", flattened)

completeDictionary :: Monad m => String -> m [String] -> CompletionFunc m
completeDictionary whitespace mxs = completeWord Nothing whitespace $ \str -> do
    xs <- mxs
    return [Completion x x False | x <- xs , str `isPrefixOf` x]
