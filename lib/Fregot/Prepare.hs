{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Prepare
    ( prepareRule
    , mergeRules

    , Imports
    , prepareQuery
    , prepareExpr
    ) where

import           Control.Applicative       ((<|>))
import           Control.Lens              (review, view, (%~), (&), (^.))
import           Control.Monad.Extended    (foldM, unless, when)
import           Control.Monad.Parachute   (ParachuteT, fatal, tellError)
import           Data.Foldable             (for_)
import qualified Data.HashMap.Strict       as HMS
import           Data.Maybe                (catMaybes, isJust, isNothing,
                                            mapMaybe)
import           Fregot.Error              (Error)
import qualified Fregot.Error              as Error
import           Fregot.Names
import           Fregot.Names.Renamer      (exprLhsAssignVars)
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens
import           Fregot.PrettyPrint        ((<+>), (<$$>))
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)
import qualified Fregot.Sugar              as Sugar
import           Prelude                   hiding (head)

-- | Create a new compiled rule from a sugared rule.
prepareRule
    :: Monad m
    => PackageName -> Imports SourceSpan -> Sugar.Rule SourceSpan Name
    -> ParachuteT Error m Rule'
prepareRule pkgname imports rule
    | head ^. Sugar.ruleDefault = do
        -- NOTE(jaspervdj): Perform sanity checks on default rules.
        when (isJust $ head ^. Sugar.ruleIndex) $ tellError $ Error.mkError
            "compile"
            (head ^. Sugar.ruleAnn)
            "bad default"
            "Default rule should not have an index associated with it."

        unless (null $ rule ^. Sugar.ruleBodies) $ tellError $ Error.mkError
            "compile"
            (head ^. Sugar.ruleAnn)
            "bad default"
            "Default rule should not have a body."

        when (head ^. Sugar.ruleAssign) $ tellError $ Error.mkError
            "compile"
            (head ^. Sugar.ruleAnn)
            "bad default"
            "Default rules should use `=` rather than `:=`."

        -- TODO(jaspervdj): About the default term, they write:
        --
        --     The term may be any scalar, composite, or comprehension value but
        --     it may not be a variable or reference. If the value is a
        --     composite then it may not contain variables or references.
        def <- traverse prepareExpr (head ^. Sugar.ruleValue)
        pure Rule
            { _rulePackage = pkgname
            , _ruleName    = head ^. Sugar.ruleName
            , _ruleKey     = review qualifiedVarFromKey (pkgname, head ^. Sugar.ruleName)
            , _ruleAnn     = head ^. Sugar.ruleAnn
            , _ruleDefault = def
            , _ruleAssign  = head ^. Sugar.ruleAssign
            , _ruleKind    = CompleteRule
            , _ruleInfo    = ()
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

        when (head ^. Sugar.ruleAssign) $ tellError $ Error.mkError
            "compile"
            (head ^. Sugar.ruleAnn)
            "bad assignment"
            "Functions should use `=` rather than `:=`."

        bodies <- traverse prepareRuleBody (rule ^. Sugar.ruleBodies)
        elses  <- traverse prepareRuleElse (rule ^. Sugar.ruleElses)
        args   <- traverse (traverse prepareTerm) (head ^. Sugar.ruleArgs)
        index  <- traverse prepareTerm (head ^. Sugar.ruleIndex)
        value  <- traverse prepareExpr (head ^. Sugar.ruleValue)
        pure Rule
            { _rulePackage = pkgname
            , _ruleName    = head ^. Sugar.ruleName
            , _ruleKey     = review qualifiedVarFromKey (pkgname, head ^. Sugar.ruleName)
            , _ruleAnn     = head ^. Sugar.ruleAnn
            , _ruleDefault = Nothing
            , _ruleAssign  = head ^. Sugar.ruleAssign
            , _ruleKind    = FunctionRule (maybe 0 length args)
            , _ruleInfo    = ()
            , _ruleDefs    =
                [ RuleDefinition
                    { _ruleDefName    = head ^. Sugar.ruleName
                    , _ruleDefImports = imports
                    , _ruleDefAnn     = head ^. Sugar.ruleAnn
                    , _ruleArgs       = args
                    , _ruleIndex      = index
                    , _ruleValue      = value
                    , _ruleBodies     = bodies
                    , _ruleElses      = elses
                    }
                ]
            }

    | otherwise = do
        let emptyDefinition =
                not (head ^. Sugar.ruleDefault) &&
                isNothing (head ^. Sugar.ruleIndex) &&
                isNothing (head ^. Sugar.ruleArgs) &&
                isNothing (head ^. Sugar.ruleValue) &&
                null (rule ^. Sugar.ruleBodies)

        when emptyDefinition $ tellError $ Error.mkError
            "compile"
            (head ^. Sugar.ruleAnn)
            "incomplete rule definition" $
            "Rules cannot be declared using just their name." <$$>
            "The rule" <+> PP.code (PP.pretty $ head ^. Sugar.ruleName) <+>
            "should have an index, arguments or a value."

        let kind
                | Nothing <- head ^. Sugar.ruleIndex = CompleteRule
                | Nothing <- head ^. Sugar.ruleValue = GenSetRule
                | otherwise                          = GenObjectRule

        -- NOTE(jaspervdj): Perform more sanity checks on rules.
        bodies <- traverse prepareRuleBody (rule ^. Sugar.ruleBodies)
        elses  <- traverse prepareRuleElse (rule ^. Sugar.ruleElses)
        args   <- traverse (traverse prepareTerm) (head ^. Sugar.ruleArgs)
        index  <- traverse prepareTerm (head ^. Sugar.ruleIndex)
        value  <- traverse prepareExpr (head ^. Sugar.ruleValue)
        pure Rule
            { _rulePackage = pkgname
            , _ruleName    = head ^. Sugar.ruleName
            , _ruleKey     = review qualifiedVarFromKey (pkgname, head ^. Sugar.ruleName)
            , _ruleAnn     = head ^. Sugar.ruleAnn
            , _ruleDefault = Nothing
            , _ruleAssign  = head ^. Sugar.ruleAssign
            , _ruleKind    = kind
            , _ruleInfo    = ()
            , _ruleDefs    =
                [ RuleDefinition
                    { _ruleDefName    = head ^. Sugar.ruleName
                    , _ruleDefImports = imports
                    , _ruleDefAnn     = head ^. Sugar.ruleAnn
                    , _ruleArgs       = args
                    , _ruleIndex      = index
                    , _ruleValue      = value
                    , _ruleBodies     = bodies
                    , _ruleElses      = elses
                    }
                ]
            }
  where
    head = rule ^. Sugar.ruleHead

-- | Merge two rules that have the same name.  This can go wrong in all sorts of
-- ways.
mergeRules
    :: (Monad m, Semigroup i)
    => Rule i SourceSpan -> Rule i SourceSpan
    -> ParachuteT Error m (Rule i SourceSpan)
mergeRules x y = do
    let defaults = mapMaybe (view ruleDefault) [x, y]
    when (length defaults > 1) $ tellError $ Error.mkMultiError
        "compile" "conflicting default"
        [ (def ^. termAnn, Just "default defined here")
        | def <- defaults
        ]

    when (x ^. ruleAssign || y ^. ruleAssign) $ tellError $ Error.mkMultiError
        "compile" "conflicting `:=` rule"
        [ (a, Just "rules declared using `:=` cannot have multiple definitions")
        | a <- [x ^. ruleAnn, y ^. ruleAnn]
        ]

    unless (compatible (x ^. ruleKind) (y ^. ruleKind)) $ tellError $
        Error.mkMultiError
            "compile" "complete definition mismatch"
            [ (c ^. ruleAnn, Just $ describeKind (c ^. ruleKind))
            | c <- [x, y]
            ]

    -- Merge y into x
    return $! x
        & ruleDefault %~ (<|> y ^. ruleDefault)
        & ruleDefs    %~ (++ y ^. ruleDefs)
        & ruleInfo    %~ (<> y ^. ruleInfo)

  where
    compatible ErrorRule _         = True
    compatible _         ErrorRule = True
    compatible k         l         = k == l

    describeKind = \case
        CompleteRule   -> "is a complete rule"
        GenSetRule     -> "generates a set"
        GenObjectRule  -> "generates an object"
        FunctionRule a -> "is a function of arity" <+> PP.pretty a
        ErrorRule      -> "broken rule"

--------------------------------------------------------------------------------

prepareRuleBody
    :: Monad m
    => Sugar.RuleBody SourceSpan Name
    -> ParachuteT Error m (RuleBody SourceSpan)
prepareRuleBody = fmap catMaybes . mapM prepareRuleStatement

prepareQuery
    :: Monad m
    => Sugar.Query SourceSpan Name -> ParachuteT Error m (Query SourceSpan)
prepareQuery = prepareRuleBody

prepareRuleElse
    :: Monad m
    => Sugar.RuleElse SourceSpan Name
    -> ParachuteT Error m (RuleElse SourceSpan)
prepareRuleElse re = RuleElse (re ^. Sugar.ruleElseAnn)
    <$> traverse prepareTerm (re ^. Sugar.ruleElseValue)
    <*> prepareRuleBody (re ^. Sugar.ruleElseBody)

-- | 'SomeS' statements are removed as we don't need them anymore, the info
-- that we are dealing with a local name is now in 'Name'.
-- TODO: actually do this for SomeInS
prepareRuleStatement
    :: Monad m
    => Sugar.RuleStatement SourceSpan Name
    -> ParachuteT Error m (Maybe (Literal SourceSpan))
prepareRuleStatement (Sugar.SomeS _ _)    = pure Nothing
prepareRuleStatement (Sugar.SomeInS source mbK v x) = do
    statement <- UnifyS source
         <$> prepareTerm v
         <*> (RefT source <$> prepareExpr x <*> (case mbK of
                 Nothing -> pure $ NameT source WildcardName
                 Just k -> prepareTerm k))
    pure . Just $ Literal
        { _literalAnn       = source
        , _literalNegation  = False
        , _literalStatement = statement
        , _literalWith      = []
        }
prepareRuleStatement (Sugar.LiteralS lit) = Just <$> prepareLiteral lit

prepareLiteral
    :: Monad m
    => Sugar.Literal SourceSpan Name
    -> ParachuteT Error m (Literal SourceSpan)
prepareLiteral slit = do
    statement <- case slit ^. Sugar.literalExpr of
        Sugar.BinOpE ann x Sugar.UnifyO y ->
            UnifyS ann <$> prepareExpr x <*> prepareExpr y
        Sugar.BinOpE ann x Sugar.AssignO y -> do
            when (isNothing $ exprLhsAssignVars x) $ tellError $ Error.mkError "compile"
                (x ^. Sugar.exprAnn)
                "invalid lhs"
                "You cannot assign to the expression to the left of `:=`"
            AssignS ann <$> prepareExpr x <*> prepareExpr y
        expr -> TermS <$> prepareExpr expr

    with <- traverse prepareWith $ slit ^. Sugar.literalWith
    pure Literal
        { _literalAnn       = slit ^. Sugar.literalAnn
        , _literalNegation  = slit ^. Sugar.literalNegation
        , _literalStatement = statement
        , _literalWith      = with
        }

prepareExpr
    :: Monad m
    => Sugar.Expr SourceSpan Name
    -> ParachuteT Error m (Term SourceSpan)
prepareExpr = \case
    Sugar.TermE _source t -> prepareTerm t
    Sugar.BinOpE source x o y -> do
        o' <- prepareBinOp source o
        x' <- prepareExpr x
        y' <- prepareExpr y
        return $ CallT source (OperatorFunction o') [x', y']
    Sugar.ParensE _source e -> prepareExpr e
    Sugar.IndRefE source x args -> do
        t <- prepareTerm x
        prepareRef source t args
    Sugar.InE source k v x -> InT source <$>
        traverse prepareExpr k <*> prepareExpr v <*> prepareExpr x

prepareTerm
    :: Monad m
    => Sugar.Term SourceSpan Name
    -> ParachuteT Error m (Term SourceSpan)
prepareTerm = \case
    Sugar.RefT source nameSource name0 refs ->
        prepareRef source (NameT nameSource name0) refs

    Sugar.CallT source names args -> case names of
        [name] -> CallT source (NamedFunction name) <$> traverse prepareExpr args
        vs -> fatal $ Error.mkError "compile" source "unknown function call" $
            "Unknown function at compile time:" <+> PP.pretty (Nested vs)

    Sugar.VarT source v -> pure $ NameT source v
    Sugar.ScalarT source s -> pure . ValueT source $ review valueToScalar s

    Sugar.ArrayT source a -> ArrayT source <$> traverse prepareExpr a
    Sugar.SetT source a -> SetT source <$> traverse prepareExpr a
    Sugar.ObjectT source obj -> do
        let duplicateKeys = HMS.filter ((> 1) . length) $ HMS.fromListWith (++)
                [(k, [src]) | (Sugar.ScalarK src k, _) <- obj]
        -- checkDups HMS.empty obj
        for_ (HMS.toList duplicateKeys) $ \(k, srcs) ->
            tellError $ Error.mkMultiError "compile"
                 ("Duplicate object key" <+> PP.code (PP.pretty k))
                 [(src, Nothing) | src <- srcs]
        ObjectT source <$> traverse prepareObjectItem obj

    Sugar.ArrayCompT ann h b -> fmap (CompT ann) $
        ArrayComp <$> prepareTerm h <*> prepareRuleBody b
    Sugar.SetCompT ann h b -> fmap (CompT ann) $
        SetComp <$> prepareTerm h <*> prepareRuleBody b
    Sugar.ObjectCompT ann k h b -> fmap (CompT ann) $ ObjectComp
        <$> prepareObjectKey k
        <*> prepareTerm h
        <*> prepareRuleBody b
    Sugar.ErrorT ann -> pure $ ErrorT ann

prepareRef
    :: Monad m
    => SourceSpan -> Term SourceSpan -> [Sugar.RefArg SourceSpan Name]
    -> ParachuteT Error m (Term SourceSpan)
prepareRef source term0 refs = foldM
    (\acc refArg -> case refArg of
        Sugar.RefDotArg ann v -> return $
            RefT source acc (review termToScalar (ann, Sugar.String (unVar v)))
        Sugar.RefBrackArg k -> do
            k' <- prepareExpr k
            return $ RefT source acc k')
    term0
    refs

prepareObjectItem
    :: Monad m
    => (Sugar.ObjectKey SourceSpan Name, Sugar.Expr SourceSpan Name)
    -> ParachuteT Error m (Term SourceSpan, Term SourceSpan)
prepareObjectItem (k, e) = (,) <$> prepareObjectKey k <*> prepareExpr e

prepareObjectKey
    :: Monad m
    => Sugar.ObjectKey SourceSpan Name
    -> ParachuteT Error m (Term SourceSpan)
prepareObjectKey = \case
    Sugar.ScalarK ann s      -> return $! ValueT ann $! review valueToScalar s
    Sugar.VarK    ann v      -> return $! NameT ann (LocalName v)
    Sugar.RefK    ann v args -> prepareRef ann (NameT ann v) args
    Sugar.ErrorK  ann        -> return $! ErrorT ann

prepareBinOp
    :: Monad m
    => SourceSpan
    -> Sugar.BinOp
    -> ParachuteT Error m BinOp
prepareBinOp source = \case
    Sugar.NotEqualO           -> pure NotEqualO
    Sugar.LessThanO           -> pure LessThanO
    Sugar.LessThanOrEqualO    -> pure LessThanOrEqualO
    Sugar.GreaterThanO        -> pure GreaterThanO
    Sugar.GreaterThanOrEqualO -> pure GreaterThanOrEqualO
    Sugar.PlusO               -> pure PlusO
    Sugar.MinusO              -> pure MinusO
    Sugar.TimesO              -> pure TimesO
    Sugar.DivideO             -> pure DivideO
    Sugar.ModuloO             -> pure ModuloO
    Sugar.BinAndO             -> pure BinAndO
    Sugar.BinOrO              -> pure BinOrO
    Sugar.UnifyO              -> pure EqualO
    Sugar.AssignO             -> do
        fatal $ Error.mkError "compile" source
            "invalid unification" $
            "The `:=` operator should not appear in this context, perhaps" <+>
            "you meant to write `==`?"

prepareWith
    :: Monad m
    => Sugar.With SourceSpan Name
    -> ParachuteT Error m (With SourceSpan)
prepareWith with = do
    term <- prepareTerm (with ^. Sugar.withAs)
    return $ With (with ^. Sugar.withAnn) (with ^. Sugar.withPath) term
