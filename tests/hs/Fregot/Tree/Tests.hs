{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Tree.Tests
    ( tests
    ) where

import           Control.Monad.Identity
import qualified Data.HashSet           as HS
import           Fregot.Tree
import qualified Test.Tasty.Extended    as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Fregot.Names.Tests"
    [ Tasty.simpleTestGroup
        "unionWithA"
        (\(lt, rt) -> HS.fromList $ toList $ runIdentity $
            unionWithA (\x y -> pure (x ++ "." ++ y)) lt rt)
        [ ( ( fromList
                [ ("data.left", "l")
                , ("data.both", "lb")
                ]
             , fromList
                [ ("data.right", "r")
                , ("data.both",  "rb")
                ]
             )
          , HS.fromList
              [ ("data.both",  "lb.rb")
              , ("data.left",  "l")
              , ("data.right", "r")
              ]
          )
        ]
    ]
