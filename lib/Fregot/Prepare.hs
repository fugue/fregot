{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Fregot.Prepare
    ( prepareRule
    , mergeRules

    , prepareImports
    , prepareExpr
    ) where


import           Control.Applicative       ((<|>))
import           Control.Lens              (view, (%~), (&), (^.))
import           Control.Monad.Extended    (foldM, unless, when)
import           Control.Monad.Parachute   (ParachuteT, fatal, tellError)
import qualified Data.HashMap.Strict       as HMS
import qualified Data.List                 as L
import           Data.Maybe                (catMaybes)
import           Data.Maybe                (isJust, isNothing, mapMaybe)
import           Fregot.Error              (Error)
import qualified Fregot.Error              as Error
import           Fregot.Names
import           Fregot.Prepare.Ast
import           Fregot.Prepare.Lens
import           Fregot.PrettyPrint        ((<+>))
import qualified Fregot.PrettyPrint        as PP
import           Fregot.Sources.SourceSpan (SourceSpan)
import qualified Fregot.Sugar              as Sugar
import           Prelude                   hiding (head)

-- | Create a new compiled rule from a sugared rule.
prepareRule
    :: Monad m
    => PackageName -> Imports SourceSpan -> Sugar.Rule SourceSpan Name
    -> ParachuteT Error m (Rule SourceSpan)
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

        -- TODO(jaspervdj): About the default term, they write:
        --
        --     The term may be any scalar, composite, or comprehension value but
        --     it may not be a variable or reference. If the value is a
        --     composite then it may not contain variables or references.
        def <- traverse prepareTerm (head ^. Sugar.ruleValue)
        pure Rule
            { _rulePackage = pkgname
            , _ruleName    = head ^. Sugar.ruleName
            , _ruleAnn     = head ^. Sugar.ruleAnn
            , _ruleDefault = def
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

        bodies <- traverse prepareRuleBody (rule ^. Sugar.ruleBodies)
        elses  <- traverse prepareRuleElse (rule ^. Sugar.ruleElses)
        args   <- traverse (traverse prepareTerm) (head ^. Sugar.ruleArgs)
        index  <- traverse prepareTerm (head ^. Sugar.ruleIndex)
        value  <- traverse prepareTerm (head ^. Sugar.ruleValue)
        pure Rule
            { _rulePackage = pkgname
            , _ruleName    = head ^. Sugar.ruleName
            , _ruleAnn     = head ^. Sugar.ruleAnn
            , _ruleDefault = Nothing
            , _ruleKind    = FunctionRule (length args)
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
        let kind
                | Nothing <- head ^. Sugar.ruleIndex = CompleteRule
                | Nothing <- head ^. Sugar.ruleValue = GenSetRule
                | otherwise                          = GenObjectRule

        -- NOTE(jaspervdj): Perform more sanity checks on rules.
        bodies <- traverse prepareRuleBody (rule ^. Sugar.ruleBodies)
        elses  <- traverse prepareRuleElse (rule ^. Sugar.ruleElses)
        args   <- traverse (traverse prepareTerm) (head ^. Sugar.ruleArgs)
        index  <- traverse prepareTerm (head ^. Sugar.ruleIndex)
        value  <- traverse prepareTerm (head ^. Sugar.ruleValue)
        pure Rule
            { _rulePackage = pkgname
            , _ruleName    = head ^. Sugar.ruleName
            , _ruleAnn     = head ^. Sugar.ruleAnn
            , _ruleDefault = Nothing
            , _ruleKind    = kind
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
        CompleteRule   -> "is a complete rule"
        GenSetRule     -> "generates a set"
        GenObjectRule  -> "generates an object"
        FunctionRule a -> "is a function of arity" <+> PP.pretty a

--------------------------------------------------------------------------------

prepareImports
    :: Monad m
    => [Sugar.Import SourceSpan]
    -> ParachuteT Error m (Imports SourceSpan)
prepareImports =
    fmap (HMS.fromList . catMaybes) . traverse prepareImport
  where
    prepareImport imp = do
        let mbAlias =
                (imp ^. Sugar.importAs) <|>
                (case unPackageName (imp ^. Sugar.importPackage) of
                    []   -> Nothing
                    bits -> return $ mkVar $ last bits)

        return $ do
            alias <- mbAlias
            return $ (alias, (imp ^. Sugar.importAnn, imp ^. Sugar.importPackage))

prepareRuleBody
    :: Monad m
    => Sugar.RuleBody SourceSpan Name
    -> ParachuteT Error m (RuleBody SourceSpan)
prepareRuleBody = mapM prepareLiteral

prepareRuleElse
    :: Monad m
    => Sugar.RuleElse SourceSpan Name
    -> ParachuteT Error m (RuleElse SourceSpan)
prepareRuleElse re = RuleElse (re ^. Sugar.ruleElseAnn)
    <$> traverse prepareTerm (re ^. Sugar.ruleElseValue)
    <*> prepareRuleBody (re ^. Sugar.ruleElseBody)

prepareLiteral
    :: Monad m
    => Sugar.Literal SourceSpan Name
    -> ParachuteT Error m (Literal SourceSpan)
prepareLiteral slit = do
    statement <- case slit ^. Sugar.literalExpr of
        Sugar.BinOpE ann x Sugar.UnifyO y ->
            UnifyS ann <$> prepareExpr x <*> prepareExpr y
        -- NOTE(jaspervdj): If there's no local name here, we obviously cannot
        -- consider it an assignment.
        Sugar.BinOpE ann (Sugar.TermE _ (Sugar.VarT _ (LocalName v))) Sugar.AssignO y ->
            AssignS ann v <$> prepareExpr y
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

prepareTerm
    :: Monad m
    => Sugar.Term SourceSpan Name
    -> ParachuteT Error m (Term SourceSpan)
prepareTerm = \case
    Sugar.RefT source nameSource name0 refs ->
        prepareRef source nameSource name0 refs

    Sugar.CallT source names args -> case names of
        [name] -> CallT source (NamedFunction name) <$> traverse prepareTerm args
        _ -> error "TODO: this doesn't seem right"

    Sugar.VarT source v -> pure $ VarT source v
    Sugar.ScalarT source s -> pure $ ScalarT source s

    Sugar.ArrayT source a -> ArrayT source <$> traverse prepareExpr a
    Sugar.SetT source a -> SetT source <$> traverse prepareExpr a
    Sugar.ObjectT source o -> ObjectT source <$> traverse prepareObjectItem o

    Sugar.ArrayCompT ann h b ->
        ArrayCompT ann <$> prepareTerm h <*> prepareRuleBody b
    Sugar.SetCompT ann h b ->
        SetCompT ann <$> prepareTerm h <*> prepareRuleBody b
    Sugar.ObjectCompT ann k h b -> ObjectCompT ann
        <$> prepareObjectKey k
        <*> prepareTerm h
        <*> prepareRuleBody b

prepareRef
    :: Monad m
    => SourceSpan -> SourceSpan -> Name -> [Sugar.RefArg SourceSpan Name]
    -> ParachuteT Error m (Term SourceSpan)
prepareRef source nameSource name0 refs = foldM
    (\acc refArg -> case refArg of
        Sugar.RefDotArg ann v -> return $
            RefT source acc (ScalarT ann (Sugar.String (unVar v)))
        Sugar.RefBrackArg k -> do
            k' <- prepareExpr k
            return $ RefT source acc k')
    (VarT nameSource name0)
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
    Sugar.ScalarK ann s      -> return $! ScalarT ann s
    Sugar.VarK    ann v      -> return $! VarT ann (LocalName v)
    Sugar.RefK    ann v args -> prepareRef ann ann v args

prepareBinOp
    :: Monad m
    => SourceSpan
    -> Sugar.BinOp
    -> ParachuteT Error m BinOp
prepareBinOp source = \case
    Sugar.EqualO              -> pure EqualO
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
    Sugar.BinOrO              -> pure BinOrO
    Sugar.UnifyO              -> do
        tellError $ Error.mkError "compile" source
            "invalid unification" $
            "The `=` operator should not appear in this context, perhaps" <+>
            "you meant to write `==`?"
        pure EqualO
    Sugar.AssignO             -> do
        tellError $ Error.mkError "compile" source
            "invalid unification" $
            "The `:=` operator should not appear in this context, perhaps" <+>
            "you meant to write `==`?"
        pure EqualO

prepareWith
    :: Monad m
    => Sugar.With SourceSpan Name
    -> ParachuteT Error m (With SourceSpan)
prepareWith with = do
    path <- case L.stripPrefix ["input"] (with ^. Sugar.withWith) of
        Just p  -> return p
        Nothing -> fatal $ Error.mkError "compile" (with ^. Sugar.withAnn)
            "invalid with"
            "The path for the with modifier should start with `input`."
    term <- prepareTerm (with ^. Sugar.withAs)
    return $ With (with ^. Sugar.withAnn) path term
