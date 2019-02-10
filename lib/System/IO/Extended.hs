module System.IO.Extended
    ( module System.IO
    , hWithEncoding
    ) where

import           Control.Exception (finally)
import           System.IO

-- | Set the encoding for the duration of an action and restore the original
-- encoding after that.
hWithEncoding :: Handle -> TextEncoding -> IO a -> IO a
hWithEncoding h encoding action = do
    mbOriginal <- hGetEncoding h
    finally
        (hSetEncoding h encoding >> action)
        (maybe (return ()) (hSetEncoding h) mbOriginal)
