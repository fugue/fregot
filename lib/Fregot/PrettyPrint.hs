{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.PrettyPrint
    ( module Fregot.PrettyPrint.Internal
    , module Fregot.PrettyPrint.Sem

    , pretty'
    , array
    , object
    ) where

import           Fregot.PrettyPrint.Internal
import           Fregot.PrettyPrint.Sem

pretty' :: Pretty Sem a => a -> SemDoc
pretty' = pretty

array :: Pretty Sem a => [a] -> SemDoc
array a = parensSepVert
    (punctuation "[") (punctuation "]") (punctuation ",")
    (map pretty a)

object :: (Pretty Sem k, Pretty Sem v) => [(k, v)] -> SemDoc
object o = parensSepVert
    (punctuation "{") (punctuation "}") (punctuation ",")
    [ pretty k <> punctuation ":" <+> pretty t
    | (k, t) <- o
    ]
