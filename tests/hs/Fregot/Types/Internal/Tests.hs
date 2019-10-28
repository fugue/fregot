{-# LANGUAGE OverloadedStrings #-}
module Fregot.Types.Internal.Tests
    ( tests
    ) where

import qualified Data.HashMap.Strict   as HMS
import qualified Fregot.Prepare.Ast    as Ast
import           Fregot.Types.Internal
import qualified Test.Tasty.Extended   as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Fregot.Types.Internal.Tests"
    [ Tasty.simpleTestGroup
        "intersectType"
        (uncurry intersectType)
        [ ((Empty, Empty), Empty)
        , ((Number, String), Empty)

        , ((Array Number, Array Any), Array Number)
        , ((Array (Or Number String), Array (Or Null String)), Array String)

        , ( ( objectOf String (Or String Number)
            , Object $ ObjectType
                (HMS.fromList [(Ast.String "name", String)])
                Nothing
            )
          , Object $ ObjectType
                (HMS.fromList [(Ast.String "name", String)])
                Nothing
          )

        , ( ( objectOf String Number
            , Object $ ObjectType
                (HMS.fromList [(Ast.String "name", String)])
                Nothing
            )
          , Empty
          )

        , ( ( Object $ ObjectType
                (HMS.fromList
                    [ (Ast.String "name", String)
                    , (Ast.String "age",  String)
                    ])
                Nothing
            , Object $ ObjectType
                (HMS.fromList
                    [ (Ast.String "name", String)
                    , (Ast.String "age",  Number)
                    ])
                Nothing
            )
          , Object $ ObjectType
                (HMS.fromList [(Ast.String "name", String)])
                Nothing
          )
        ]
    ]
