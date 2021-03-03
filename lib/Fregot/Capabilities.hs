{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX

Encoding builtin information to a capabilities document.
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Capabilities
    ( renderCapabilities
    ) where

import           Control.Lens             ((^?))
import qualified Data.Aeson               as A
import qualified Data.HashMap.Strict      as HMS
import qualified Data.HashSet             as HS
import           Data.List                (sort, sortOn)
import qualified Data.Text                as T
import           Fregot.Builtins.Internal
import           Fregot.Names             (nameToText)
import           Fregot.Prepare.Ast       (BinOp (..), binOpToText)
import qualified Fregot.Types.Builtins    as Ty
import qualified Fregot.Types.Internal    as Ty

renderCapabilities :: Builtins m -> A.Value
renderCapabilities builtins = A.object
    [ "builtins" A..= renderBuiltins builtins
    ]

renderBuiltins :: Builtins m -> A.Value
renderBuiltins = A.toJSON . map (uncurry renderBuiltin) .
    sortOn (functionName . fst) . HMS.toList

functionName :: Function -> T.Text
functionName = \case
    NamedFunction name  -> nameToText name
    OperatorFunction op -> operatorName op

renderBuiltin :: Function -> Builtin m -> A.Value
renderBuiltin fn (Builtin bt _) = A.object $
    [ "decl" A..= renderDecl (Ty.btRepr bt)
    , "name" A..= functionName fn
    ] ++ case fn of
        NamedFunction    _  -> []
        OperatorFunction op -> ["infix" A..= binOpToText op]

operatorName :: BinOp -> T.Text
operatorName = \case
    EqualO              -> "eq"
    NotEqualO           -> "neq"
    LessThanO           -> "lt"
    LessThanOrEqualO    -> "lte"
    GreaterThanO        -> "gt"
    GreaterThanOrEqualO -> "gte"
    PlusO               -> "plus"
    MinusO              -> "minus"
    TimesO              -> "mul"
    DivideO             -> "div"
    ModuloO             -> "rem"
    BinAndO             -> "and"
    BinOrO              -> "or"

renderDecl :: Ty.TypeRepr i o -> A.Value
renderDecl typeRepr = A.object
    [ typeTag "function"
    , "args"   A..= map renderType args
    , "result" A..= renderType ret
    ]
  where
    (args, ret) = Ty.unTypeRepr typeRepr

renderType :: Ty.Type -> A.Value
renderType = \case
    t | Just e <- t ^? Ty.singleton -> renderElemType e
    Ty.Universe                     -> anyType
    Ty.Unknown                      -> anyType
    Ty.Union multi                  -> A.object
        [typeTag "any", "of" A..= fmap renderElemType (sort $ HS.toList multi)]

renderElemType :: Ty.Elem Ty.Type -> A.Value
renderElemType = \case
    Ty.Boolean  -> A.object [typeTag "boolean"]
    Ty.Number   -> A.object [typeTag "number"]
    Ty.Null     -> A.object [typeTag "null"]
    Ty.String   -> A.object [typeTag "string"]
    Ty.Scalar s -> renderType $ Ty.scalarType s
    Ty.Set e    -> A.object [typeTag "set", "of" A..= renderType e]
    Ty.Array sd -> A.object
        -- TODO (jaspervdj): Expand `static` parts.
        [ typeTag "array", "dynamic" A..= case Ty.sdDynamic sd of
            Just (_, ety) -> renderType ety
            _             -> anyType
        ]
    Ty.Object sd -> A.object
        -- TODO (jaspervdj): Expand `static` parts.
        [ typeTag "array", "dynamic" A..= case Ty.sdDynamic sd of
            Just (kty, vty) -> A.object
                ["key" A..= renderType kty, "value" A..= renderType vty]
            _               -> A.object
                ["key" A..= anyType, "value" A..= anyType]
        ]

anyType :: A.Value
anyType = A.object [typeTag "any"]

typeTag :: A.KeyValue kv => T.Text -> kv
typeTag = ("type" A..=)
