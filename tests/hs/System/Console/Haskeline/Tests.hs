module System.Console.Haskeline.Tests
    ( tests
    ) where

import           Control.Monad.Identity            (runIdentity)
import           System.Console.Haskeline.Extended
import qualified Test.Tasty                        as Tasty
import           Test.Tasty.HUnit                  ((@?=))
import qualified Test.Tasty.HUnit                  as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "System.Console.Haskeline.Tests"
    [ Tasty.testGroup "concatCompletion"
        [ Tasty.testCase "01" $
            let completion = concatCompletion $
                    [ noCompletion
                    , completeWord Nothing " " $ \s -> case s of
                        "wat" -> return [simpleCompletion "waddup"]
                        _     -> return []
                    ] in

            snd (runIdentity (completion ("taw", ""))) @?=
            [ Completion "waddup" "waddup" True
            ]
        ]
    ]
