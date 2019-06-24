module Debug.Trace.Extended
    ( module Debug.Trace
    , traceLines
    ) where

import           Debug.Trace

traceLines :: [String] -> a -> a
traceLines l = trace (unlines l)
