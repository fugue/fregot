{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Prepare
    ( prepareRule
    , mergeRules
    ) where


import           Control.Applicative       ((<|>))
import           Control.Lens              (view, (%~), (&), (^.))
import           Control.Monad.Extended    (unless, when)
import           Control.Monad.Parachute   (ParachuteT, tellError)
import           Data.Maybe                (isJust, isNothing, mapMaybe)
import           Fregot.Error              (Error)
import qualified Fregot.Error              as Error
import           Fregot.Prepare.AST
import           Fregot.Sources.SourceSpan (SourceSpan)
import qualified Fregot.Sugar              as Sugar
import           Prelude                   hiding (head)

{-
toUnification :: Sugar.Expr a -> Expr a
toUnification (Sugar.BinOpE a x Sugar.UnifyO y) =
    UnifyE a x y
toUnification (Sugar.BinOpE a (Sugar.TermE _ (Sugar.VarT _ v)) Sugar.AssignO x) =
    AssignE a v x
toUnification e                                         = fail "wat"
-}
-- | Create a new compiled rule from a sugared rule.
prepareRule
    :: Monad m
    => [Sugar.Import SourceSpan] -> Sugar.Rule SourceSpan
    -> ParachuteT Error m (Rule SourceSpan)
prepareRule imports rule
    | head ^. Sugar.ruleDefault = do
        -- NOTE(jaspervdj): Perform sanity checks on default rules.
        when (isJust $ head ^. Sugar.ruleIndex) $ tellError $ Error.mkError
            "compile"
            (head ^. Sugar.ruleAnn)
            "bad default"
            "Default rule should not have an index associated with it."

        unless (null $ rule ^. Sugar.ruleBody) $ tellError $ Error.mkError
            "compile"
            (head ^. Sugar.ruleAnn)
            "bad default"
            "Default rule should not have a body."

        -- TODO(jaspervdj): About the default term, they write:
        --
        --     The term may be any scalar, composite, or comprehension value but
        --     it may not be a variable or reference. If the value is a
        --     composite then it may not contain variables or references.
        pure Rule
            { _ruleName    = head ^. Sugar.ruleName
            , _ruleAnn     = head ^. Sugar.ruleAnn
            , _ruleDefault = head ^. Sugar.ruleValue
            , _ruleKind    = CompleteRule
            , _ruleDefs    = []
            }

    | not (null (head ^. Sugar.ruleArgs)) = do
        -- It's a function.
        unless (isNothing $ head ^. Sugar.ruleIndex) $ tellError $ Error.mkError
            "compile"
            (head ^. Sugar.ruleAnn)
            "invalid function" $
            "Rule should have function arguments, " <>
            "or regular arguments, but not both."

        pure Rule
            { _ruleName    = head ^. Sugar.ruleName
            , _ruleAnn     = head ^. Sugar.ruleAnn
            , _ruleDefault = Nothing
            , _ruleKind    = FunctionRule
            , _ruleDefs    =
                [ RuleDefinition
                    { _ruleDefName    = head ^. Sugar.ruleName
                    , _ruleDefImports = imports
                    , _ruleDefAnn     = head ^. Sugar.ruleAnn
                    , _ruleArgs       = head ^. Sugar.ruleArgs
                    , _ruleIndex      = head ^. Sugar.ruleIndex
                    , _ruleValue      = head ^. Sugar.ruleValue
                    , _ruleBody       = rule ^. Sugar.ruleBody
                    }
                ]
            }

    | otherwise = do
        let kind
                | Nothing <- head ^. Sugar.ruleIndex = CompleteRule
                | Nothing <- head ^. Sugar.ruleValue = GenSetRule
                | otherwise                          = GenObjectRule

        -- NOTE(jaspervdj): Perform sanity checks on rules.
        pure Rule
            { _ruleName    = head ^. Sugar.ruleName
            , _ruleAnn     = head ^. Sugar.ruleAnn
            , _ruleDefault = Nothing
            , _ruleKind    = kind
            , _ruleDefs    =
                [ RuleDefinition
                    { _ruleDefName    = head ^. Sugar.ruleName
                    , _ruleDefImports = imports
                    , _ruleDefAnn     = head ^. Sugar.ruleAnn
                    , _ruleArgs       = head ^. Sugar.ruleArgs
                    , _ruleIndex      = head ^. Sugar.ruleIndex
                    , _ruleValue      = head ^. Sugar.ruleValue
                    , _ruleBody       = rule ^. Sugar.ruleBody
                    }
                ]
            }
  where
    head = rule ^. Sugar.ruleHead

-- | Merge two rules that have the same name.  This can go wrong in all sorts of
-- ways.
mergeRules
    :: Monad m
    => Rule SourceSpan -> Rule SourceSpan
    -> ParachuteT Error m (Rule SourceSpan)
mergeRules x y = do
    let defaults = mapMaybe (view ruleDefault) [x, y]
    when (length defaults > 1) $ tellError $ Error.mkMultiError
        "compile" "conflicting default"
        [ (def ^. termAnn, "default defined here")
        | def <- defaults
        ]

    when (x ^. ruleKind /= y ^. ruleKind) $ tellError $
        Error.mkMultiError
            "compile" "complete definition mismatch"
            [ (c ^. ruleAnn, describeKind (c ^. ruleKind))
            | c <- [x, y]
            ]

    -- Merge y into x
    return $! x
        & ruleDefault %~ (<|> y ^. ruleDefault)
        & ruleDefs    %~ (++ y ^. ruleDefs)

  where
    describeKind = \case
        CompleteRule  -> "is a complete rule"
        GenSetRule    -> "generates a set"
        GenObjectRule -> "generates an object"
        FunctionRule  -> "is a function"

