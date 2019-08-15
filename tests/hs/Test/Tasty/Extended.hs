module Test.Tasty.Extended
    ( module Test.Tasty
    , simpleTestGroup
    ) where

import           Test.Tasty
import           Test.Tasty.HUnit

simpleTestGroup :: (Eq o, Show o) => String -> (i -> o) -> [(i, o)] -> TestTree
simpleTestGroup name f cases = testGroup name
    [ testCase (show idx) $ f input @?= output
    | (idx, (input, output)) <- zip [1 :: Int ..] cases
    ]
