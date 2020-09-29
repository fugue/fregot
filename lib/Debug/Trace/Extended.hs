{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
module Debug.Trace.Extended
    ( module Debug.Trace
    , traceLines
    ) where

import           Debug.Trace

traceLines :: [String] -> a -> a
traceLines l = trace (unlines l)
