{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
module Language.Dot
    ( Dot
    , writeDot
    , digraph
    ) where

newtype Dot = Dot [String]

writeDot :: FilePath -> Dot -> IO ()
writeDot path (Dot ls) = writeFile path (unlines ls)

digraph
    :: (node -> String)
    -> (key -> String)
    -> [(node, key, [key])]
    -> Dot
digraph nodeToString keyToString vertices = Dot $
    ["digraph {"] ++
    concat
        [ [keyToString k ++ "[label=" ++ show (nodeToString n) ++ "];"] ++
          [keyToString k ++ " -> " ++ keyToString o ++ ";" | o <- os]
        | (n, k, os) <- vertices
        ] ++
    ["}"]
