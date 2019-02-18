{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Interpreter.Package
    ( Package (..), packageName, packageRules
    , RuleKind (..)
    , CompiledRule (..), cruleDefault, cruleKind, cruleDefs
    , RuleDefinition (..), ruleDefImports, ruleDefRule

    , empty
    , insert
    , lookup
    , rules
    ) where

import           Control.Applicative       ((<|>))
import           Control.Lens              (view, (%~), (&), (^.))
import           Control.Lens.TH           (makeLenses)
import           Control.Monad             (unless, when)
import           Control.Monad.Parachute
import qualified Data.HashMap.Strict       as HMS
import           Data.Maybe                (mapMaybe)
import           Data.Maybe                (isJust)
import           Fregot.Error              (Error)
import           Fregot.Error              as Error
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Fregot.Sugar
import           Prelude                   hiding (head, lookup)

data Package = Package
    { _packageName  :: !PackageName
    , _packageRules :: !(HMS.HashMap Var CompiledRule)
    } deriving (Show)

data RuleKind
    = CompleteRule
    | GenSetRule
    | GenObjectRule
    deriving (Eq, Show)

data CompiledRule = CompiledRule
    { _cruleAnn     :: !SourceSpan
    , _cruleDefault :: !(Maybe (Term SourceSpan))
    , _cruleKind    :: !RuleKind
    , _cruleDefs    :: [RuleDefinition]
    } deriving (Show)

data RuleDefinition = RuleDefinition
    { _ruleDefImports :: ![Import SourceSpan]
    , _ruleDefRule    :: !(Rule SourceSpan)
    } deriving (Show)

$(makeLenses ''Package)
$(makeLenses ''CompiledRule)
$(makeLenses ''RuleDefinition)

empty :: PackageName -> Package
empty name = Package
    { _packageName  = name
    , _packageRules = HMS.empty
    }

-- | Create a new compiled rule from a sugared rule.
compileRule
    :: Monad m
    => [Import SourceSpan] -> Rule SourceSpan
    -> ParachuteT Error m CompiledRule
compileRule imports rule
    | head ^. ruleDefault = do
        -- NOTE(jaspervdj): Perform sanity checks on default rules.
        when (isJust $ head ^. ruleIndex) $ tellError $ Error.mkError
            "compile"
            (head ^. ruleAnn)
            "bad default"
            "Default rule should not have an index associated with it."

        unless (null $ rule ^. ruleBody) $ tellError $ Error.mkError
            "compile"
            (head ^. ruleAnn)
            "bad default"
            "Default rule should not have a body."

        -- TODO(jaspervdj): About the default term, they write:
        --
        --     The term may be any scalar, composite, or comprehension value but
        --     it may not be a variable or reference. If the value is a
        --     composite then it may not contain variables or references.
        pure CompiledRule
            { _cruleAnn     = head ^. ruleAnn
            , _cruleDefault = head ^. ruleValue
            , _cruleKind    = CompleteRule
            , _cruleDefs    = []
            }

    | otherwise = do
        let kind
                | Nothing <- head ^. ruleIndex = CompleteRule
                | Nothing <- head ^. ruleValue = GenSetRule
                | otherwise                    = GenObjectRule

        -- NOTE(jaspervdj): Perform sanity checks on rules.
        pure CompiledRule
            { _cruleAnn     = head ^. ruleAnn
            , _cruleDefault = Nothing
            , _cruleKind    = kind
            , _cruleDefs    = [RuleDefinition imports rule]
            }
  where
    head = rule ^. ruleHead

-- | Merge two rules that have the same name.  This can go wrong in all sorts of
-- ways.
mergeRules
    :: Monad m
    => CompiledRule -> CompiledRule
    -> ParachuteT Error m CompiledRule
mergeRules x y = do
    let defaults = mapMaybe (view cruleDefault) [x, y]
    when (length defaults > 1) $ tellError $ Error.mkMultiError
        "compile" "conflicting default"
        [ (def ^. termAnn, "default defined here")
        | def <- defaults
        ]

    when (x ^. cruleKind /= y ^. cruleKind) $ tellError $
        Error.mkMultiError
            "compile" "complete definition mismatch"
            [ (c ^. cruleAnn, describeKind (c ^. cruleKind))
            | c <- [x, y]
            ]

    -- Merge y into x
    return $! x
        & cruleDefault %~ (<|> y ^. cruleDefault)
        & cruleDefs    %~ (++ y ^. cruleDefs)

  where
    describeKind = \case
        CompleteRule  -> "is a complete rule"
        GenSetRule    -> "generates a set"
        GenObjectRule -> "generates an object"

-- | Add a new rule.
insert
    :: Monad m
    => [Import SourceSpan]
    -> Rule SourceSpan
    -> Package
    -> ParachuteT Error m Package
insert imports rule package = do
    new <- compileRule imports rule
    merged <- case HMS.lookup rname (package ^. packageRules) of
        Nothing  -> return new
        Just old -> mergeRules old new

    return $ package & packageRules %~ HMS.insert rname merged
  where
    rname = rule ^. ruleHead . ruleName

lookup :: Var -> Package -> Maybe CompiledRule
lookup var pkg = HMS.lookup var (pkg ^. packageRules)

rules :: Package -> [Var]
rules = map fst . HMS.toList . view packageRules
