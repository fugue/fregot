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
        (ruleBodyTerms . termCosmosClosures . termCosmosVars)
        [ literal $ UnifyS () (v "a") (v "b")
        , literal $ TermS (ArrayCompT () (v "c") [literal $ TermS (v "c")])
        , literal $ TermS $ ArrayT ()
            [ ScalarT () (String "wat")
            , ArrayCompT () (v "d") [literal $ TermS (v "d")]
            , ArrayCompT () (v "e") [literal $ TermS (v "e")]
            ]
        ] @?= ["c", "d", "e"]
    , Tasty.testCase "vars not in closures" $ HS.toHashSetOf
        (ruleBodyTerms . termCosmosNoClosures . termVars)
        [ literal $ UnifyS () (v "a") (v "b")
        , literal $ TermS (ArrayCompT () (v "c") [literal $ TermS (v "c")])
        , literal $ TermS $ ArrayT ()
            [ ScalarT () (String "wat")
            , ArrayCompT () (v "d") [literal $ TermS (v "d")]
            , ArrayCompT () (v "e") [literal $ TermS (v "e")]
            , v "dontforgetme"
            ]
        , literal $ UnifyS () (v "x") $ ArrayT () [v "y", v "z"]
        ] @?= ["a", "b", "x", "y", "z", "dontforgetme"]
    ]
  where
    v = VarT ()
