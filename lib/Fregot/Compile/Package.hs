{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Compile.Package
    ( CompiledPackage
    , lookup
    , rules
    , compile
    ) where

import           Control.Monad.Parachute (ParachuteT)
import           Fregot.Error            (Error)
import           Fregot.Prepare.Package
import           Prelude                 hiding (head, lookup)

type CompiledPackage = Package ()

compile :: Monad m => PreparedPackage -> ParachuteT Error m CompiledPackage
compile pkg = return pkg
