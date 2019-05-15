{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Prepare.Package
    ( PreparedPackage
    , Package (..), packageName, packageRules
    , empty
    , insert
    , lookup
    , rules
    ) where

import           Control.Lens              (view, (%~), (&), (^.))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad.Parachute
import qualified Data.HashMap.Strict       as HMS
import           Fregot.Error              (Error)
import           Fregot.Names
import           Fregot.Prepare
import           Fregot.Prepare.Ast
import           Fregot.Sources.SourceSpan (SourceSpan)
import qualified Fregot.Sugar              as Sugar
import           Prelude                   hiding (head, lookup)

type PreparedPackage = Package ()

data Package ty = Package
    { _packageName  :: !PackageName
    , _packageRules :: !(HMS.HashMap Var (Rule SourceSpan))
    } deriving (Show)

$(makeLenses ''Package)

empty :: PackageName -> Package ty
empty name = Package
    { _packageName  = name
    , _packageRules = HMS.empty
    }

-- | Add a new rule.
insert
    :: Monad m
    => Imports SourceSpan
    -> Sugar.Rule SourceSpan Name
    -> Package ty
    -> ParachuteT Error m (Package ty)
insert imports rule package = do
    new <- prepareRule (package ^. packageName) imports rule
    merged <- case HMS.lookup rname (package ^. packageRules) of
        Nothing  -> return new
        Just old -> mergeRules old new

    return $ package & packageRules %~ HMS.insert rname merged
  where
    rname = rule ^. Sugar.ruleHead . Sugar.ruleName

lookup :: Var -> Package ty -> Maybe (Rule SourceSpan)
lookup var pkg = HMS.lookup var (pkg ^. packageRules)

rules :: Package ty -> [Var]
rules = map fst . HMS.toList . view packageRules
