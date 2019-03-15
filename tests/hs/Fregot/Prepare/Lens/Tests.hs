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
    ]

  where
    v = VarT ()
