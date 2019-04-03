{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Prepare.Lens.Tests where

import qualified Data.HashSet.Extended as HS
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens
import qualified Test.Tasty            as Tasty
import           Test.Tasty.HUnit      ((@?=))
import qualified Test.Tasty.HUnit      as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Fregot.Prepare.Lens.Tests"
    [ Tasty.testCase "vars in closures" $ HS.toHashSetOf
        (ruleBodyTerms . termCosmosClosures . termCosmosVars . traverse)
        [ l $ UnifyS () (v "a") (v "b")
        , l $ TermS (ArrayCompT () (v "c") [l $ TermS (v "c")])
        , l $ TermS $ ArrayT ()
            [ ScalarT () (String "wat")
            , ArrayCompT () (v "d") [l $ TermS (v "d")]
            , ArrayCompT () (v "e") [l $ TermS (v "e")]
            ]
        ] @?= ["c", "d", "e"]
    , Tasty.testCase "vars not in closures" $ HS.toHashSetOf
        (ruleBodyTerms . termCosmosNoClosures . termVars . traverse)
        [ l $ UnifyS () (v "a") (v "b")
        , l $ TermS (ArrayCompT () (v "c") [l $ TermS (v "c")])
        , l $ TermS $ ArrayT ()
            [ ScalarT () (String "wat")
            , ArrayCompT () (v "d") [l $ TermS (v "d")]
            , ArrayCompT () (v "e") [l $ TermS (v "e")]
            , v "dontforgetme"
            ]
        , l $ UnifyS () (v "x") $ ArrayT () [v "y", v "z"]
        ] @?= ["a", "b", "x", "y", "z", "dontforgetme"]
    ]
  where
    v = VarT ()
    l = literal ()
