{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Prepare.Lens.Tests where

import qualified Data.HashSet.Extended as HS
import           Fregot.Names          (_LocalName)
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Dsl    ()
import           Fregot.Prepare.Lens
import           Fregot.Eval.Value     (Value (..), ValueF (..))
import qualified Test.Tasty            as Tasty
import           Test.Tasty.HUnit      ((@?=))
import qualified Test.Tasty.HUnit      as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Fregot.Prepare.Lens.Tests"
    [ Tasty.testCase "vars in closures" $ HS.toHashSetOf
        (ruleBodyTerms . termCosmosClosures . comprehensionTerms . termCosmosNames . traverse . _LocalName)
        [ l $ UnifyS () (v "a") (v "b")
        , l $ TermS $ CompT () $ ArrayComp (v "c") [l $ TermS (v "c")]
        , l $ TermS $ ArrayT ()
            [ ValueT () (Value $ StringV "wat")
            , CompT () $ ArrayComp (v "d") [l $ TermS (v "d")]
            , CompT () $ ArrayComp (v "e") [l $ TermS (v "e")]
            ]
        ] @?= ["c", "d", "e"]
    , Tasty.testCase "vars not in closures" $ HS.toHashSetOf
        (ruleBodyTerms . termCosmosNoClosures . termNames . traverse . _LocalName)
        [ l $ UnifyS () (v "a") (v "b")
        , l $ TermS $ CompT () $ ArrayComp (v "c") [l $ TermS (v "c")]
        , l $ TermS $ ArrayT ()
            [ ValueT () (Value $ StringV "wat")
            , CompT () $ ArrayComp (v "d") [l $ TermS (v "d")]
            , CompT () $ ArrayComp (v "e") [l $ TermS (v "e")]
            , v "dontforgetme"
            ]
        , l $ UnifyS () (v "x") $ ArrayT () [v "y", v "z"]
        ] @?= ["a", "b", "x", "y", "z", "dontforgetme"]
    ]
  where
    v = NameT ()
    l = literal ()
