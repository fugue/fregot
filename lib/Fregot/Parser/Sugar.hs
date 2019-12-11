{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Fregot.Parser.Sugar
    ( ParserOptions, poDefaultPackageName
    , defaultParserOptions

    , parseModule
    , rule
    , query
    , expr
    ) where

import           Control.Applicative         ((<|>))
import           Control.Lens                ((^.))
import           Control.Lens.TH             (makeLenses)
import           Data.Either                 (partitionEithers)
import           Data.Functor                (($>))
import qualified Data.Scientific             as Scientific
import           Fregot.Parser.Internal
import qualified Fregot.Parser.Token         as Tok
import qualified Fregot.PrettyPrint          as PP
import           Fregot.Sources.SourceSpan
import           Fregot.Sugar
import           Prelude                     hiding (head)
import qualified Text.Parsec.Expr            as Parsec
import           Text.Parsec.Extended        ((<?>))
import qualified Text.Parsec.Extended        as Parsec
import qualified Text.Parsec.Indent.Explicit as Indent

data ParserOptions = ParserOptions
    { -- | A default package name which is used if there is no package name
      -- declaration in the module.  If this is not set, a package name
      -- declaration is required.
      _poDefaultPackageName :: Maybe PackageName
    } deriving (Show)

$(makeLenses ''ParserOptions)

defaultParserOptions :: ParserOptions
defaultParserOptions = ParserOptions
    { _poDefaultPackageName = Nothing
    }

parsePackageName :: FregotParser PackageName
parsePackageName =
    mkPackageName <$> Parsec.sepBy1 Tok.var (Tok.symbol Tok.TPeriod)

parseImportGut :: FregotParser ImportGut
parseImportGut = do
    pos     <- Parsec.getPosition
    pkgname <- parsePackageName
    case unPackageName pkgname of
        ("data"  : vs) -> return $! ImportData  (mkPackageName vs)
        ("input" : vs) -> return $! ImportInput (mkPackageName vs)
        _             -> Parsec.unexpectedAt pos $
            show (PP.pretty' pkgname) ++
            " (imports should start with `data.` or `input.`)"

parseModule :: ParserOptions -> FregotParser (Module SourceSpan Var)
parseModule po = withSourceSpan $ do
    _modulePackage <- parseModuleHead po
    _moduleImports <- Parsec.many parseModuleImport
    _modulePolicy  <- Parsec.many rule
    pure $ \_moduleAnn -> Module {..}

parseModuleHead :: ParserOptions -> FregotParser PackageName
parseModuleHead po = case po ^. poDefaultPackageName of
    Nothing  -> parser
    Just def -> Parsec.option def parser
  where
    parser = (<?> "'package' declaration") $
        Tok.symbol Tok.TPackage *> parsePackageName

parseModuleImport :: FregotParser (Import SourceSpan)
parseModuleImport = withSourceSpan $ do
    Tok.symbol Tok.TImport
    _importGut <- parseImportGut
    _importAs  <- Parsec.optionMaybe $ do
        Tok.symbol Tok.TAs
        var
    return $ \_importAnn -> Import {..}

var :: FregotParser Var
var = mkVar <$> Tok.var

rule :: FregotParser (Rule SourceSpan Var)
rule = Rule
    <$> parseRuleHead
    <*> Parsec.many parseRuleBody
    <*> Parsec.many parseRuleElse

parseRuleHead :: FregotParser (RuleHead SourceSpan Var)
parseRuleHead = withSourceSpan $ do
    _ruleDefault <- Parsec.option False $ Tok.symbol Tok.TDefault $> True
    _ruleName <- var
    _ruleArgs <- Parsec.optionMaybe $
        sepTrailing Tok.TLParen Tok.TRParen Tok.TComma term
    _ruleIndex <- Parsec.optionMaybe $ do
        Tok.symbol Tok.TLBracket
        t <- term
        expectToken Tok.TRBracket
        return t
    _ruleValue <- Parsec.optionMaybe $ do
        Tok.symbol Tok.TUnify
        term
    return $ \_ruleAnn -> RuleHead {..}

parseRuleBody :: FregotParser (RuleBody SourceSpan Var)
parseRuleBody = do
    Tok.symbol Tok.TLBrace
    body <- query
    Tok.symbol Tok.TRBrace
    return body

parseRuleElse :: FregotParser (RuleElse SourceSpan Var)
parseRuleElse = withSourceSpan $ do
    Tok.symbol Tok.TElse
    _ruleElseValue <- Parsec.optionMaybe $ do
        Tok.symbol Tok.TUnify
        term
    _ruleElseBody <- parseRuleBody
    return $ \_ruleElseAnn -> RuleElse {..}

query :: FregotParser (RuleBody SourceSpan Var)
query = blockOrSemi ruleStatement

-- | Parse either a block of lines, or lines separated by a semicolon, or both.
blockOrSemi :: FregotParser a -> FregotParser [a]
blockOrSemi linep =
    (do
        pos <- Parsec.getPosition
        l   <- linep
        (l :) <$> go pos) <|>
    (return [])
  where
    go pos0 =
        (do
            Tok.symbol Tok.TSemicolon
            pos1 <- Parsec.getPosition
            l    <- linep
            (l :) <$> go pos1) <|>
        (do
            pos1 <- Parsec.getPosition
            if Parsec.sourceLine pos1 <= Parsec.sourceLine pos0
                then Parsec.parserFail "expected newline before next statement"
                else do
                    l <- linep
                    (l :) <$> go pos1) <|>
        return []

ruleStatement :: FregotParser (RuleStatement SourceSpan Var)
ruleStatement =
    (withSourceSpan $ do
        Tok.symbol Tok.TSome
        vars <- Parsec.sepBy1 var (Tok.symbol Tok.TPeriod)
        return $ \ann -> VarDeclS ann vars) <|>
     (LiteralS <$> literal)

literal :: FregotParser (Literal SourceSpan Var)
literal = withSourceSpan $ do
    _literalNegation <- Parsec.option False $ Tok.symbol Tok.TNot $> True
    _literalExpr <- expr
    _literalWith <- Parsec.many parseWith
    return $ \_literalAnn -> Literal {..}

expr :: FregotParser (Expr SourceSpan Var)
expr = Parsec.buildExpressionParser
    [ [ binary Tok.TTimes  TimesO  Parsec.AssocLeft
      , binary Tok.TDivide DivideO Parsec.AssocLeft
      , binary Tok.TModulo ModuloO Parsec.AssocLeft
      ]

    , [ binary Tok.TPlus  PlusO  Parsec.AssocLeft
      , binary Tok.TMinus MinusO Parsec.AssocLeft
      ]

    , [ binary Tok.TLessThan           LessThanO           Parsec.AssocLeft
      , binary Tok.TLessThanOrEqual    LessThanOrEqualO    Parsec.AssocLeft
      , binary Tok.TGreaterThan        GreaterThanO        Parsec.AssocLeft
      , binary Tok.TGreaterThanOrEqual GreaterThanOrEqualO Parsec.AssocLeft
      ]

    , [ binary Tok.TBinAnd BinAndO Parsec.AssocLeft ]
    , [ binary Tok.TPipe   BinOrO  Parsec.AssocLeft ]

    , [ binary Tok.TEqual    EqualO    Parsec.AssocLeft
      , binary Tok.TNotEqual NotEqualO Parsec.AssocLeft
      ]

    , [ binary Tok.TUnify  UnifyO  Parsec.AssocRight ]
    , [ binary Tok.TAssign AssignO Parsec.AssocRight ]
    ]
    simpleExpr
  where
    simpleExpr = withSourceSpan $
        (do
            Tok.symbol Tok.TLParen
            e <- expr
            expectToken Tok.TRParen
            return $ \ss -> ParensE ss e) <|>
        (do
            t <- term
            return $ \ss -> TermE ss t)

    binary tok op = Parsec.Infix (do
        Tok.symbol tok
        return $ \x y ->
            BinOpE (unsafeMergeSourceSpan (x ^. exprAnn) (y ^. exprAnn)) x op y)

term :: FregotParser (Term SourceSpan Var)
term = withSourceSpan $
    (do
        startI   <- Indent.indentation
        (v, vss) <- withSourceSpan $ var >>= \v -> return $ \vss -> (v, vss)
        refArgs  <- Parsec.option [] $ Indent.same startI >> Parsec.many refArg

        let isDotArg (RefDotArg _ _) = True
            isDotArg (RefBrackArg _) = False

        mbCallArgs <- case all isDotArg refArgs of
            False -> return Nothing
            True  -> Parsec.optionMaybe $
                sepTrailing Tok.TLParen Tok.TRParen Tok.TComma expr

        return $ \ss -> case (refArgs, mbCallArgs) of
            ([], Nothing)   -> VarT ss v
            (_,  Nothing)   -> RefT ss vss v refArgs
            (_, Just cargs) -> CallT ss
                (v : [v' | RefDotArg _ v' <- refArgs])
                cargs) <|>
    (do
        s <- scalar
        return $ \ss -> ScalarT ss s) <|>
    (do
        (x, body) <- comprehension Tok.TLBracket Tok.TRBracket term
        return $ \ss -> ArrayCompT ss x body) <|>
    (do
        a <- array
        return $ \ss -> ArrayT ss a) <|>
    (do
        (x, body) <- comprehension Tok.TLBrace Tok.TRBrace term
        return $ \ss -> SetCompT ss x body) <|>
    (do
        ((k, x), body) <- comprehension Tok.TLBrace Tok.TRBrace $ do
            k0 <- objectKey
            Tok.symbol Tok.TColon
            x0 <- term
            return (k0, x0)
        return $ \ss -> ObjectCompT ss k x body) <|>
    (do
        os <- objectOrSet
        return $ \ss -> case os of
            Left  o -> ObjectT ss o
            Right s -> SetT ss s)

refArg :: FregotParser (RefArg SourceSpan Var)
refArg =
    (do
        Tok.symbol Tok.TLBracket
        e <- expr
        expectToken Tok.TRBracket
        return $ RefBrackArg e) <|>
    (withSourceSpan $ do
        Tok.symbol Tok.TPeriod
        v <- var
        return $ \ss -> RefDotArg ss v)

scalar :: FregotParser Scalar
scalar =
    (String <$> Tok.string) <|>
    (do
        negation   <- Parsec.option False (True <$ Tok.symbol Tok.TMinus)
        intOrFloat <- Tok.intOrFloat
        return $! Number $! (if negation then negate else id) $
            case intOrFloat of
                Left  x -> fromIntegral x
                Right x -> Scientific.fromFloatDigits x) <|>
    (pure (Bool True) <* Tok.symbol Tok.TTrue) <|>
    (pure (Bool False) <* Tok.symbol Tok.TFalse) <|>
    (pure Null <* Tok.symbol Tok.TNull)

array :: FregotParser [Expr SourceSpan Var]
array = sepTrailing Tok.TLBracket Tok.TRBracket Tok.TComma expr

objectOrSet
    :: FregotParser (Either (Object SourceSpan Var) [Expr SourceSpan Var])
objectOrSet = do
    items <- sepTrailing Tok.TLBrace Tok.TRBrace Tok.TComma item
    case partitionEithers items of
        ([], setitems) -> return $ Right setitems
        (objitems, []) -> return $ Left objitems
        _              -> Parsec.unexpected "mixed object and set"
  where
    item =
        (do
            k <- Parsec.try $ objectKey <* Tok.symbol Tok.TColon
            e <- expr
            return $ Left (k, e)) <|>
        (do
            fmap Right expr)

objectKey :: FregotParser (ObjectKey SourceSpan Var)
objectKey = withSourceSpan $
    (do
        s <- scalar
        return $ \ss -> ScalarK ss s) <|>
    (do
        v       <- var
        refArgs <- Parsec.many refArg
        return $ \ss -> case refArgs of
            [] -> VarK ss v
            _  -> RefK ss v refArgs)

parseWith :: FregotParser (With SourceSpan Var)
parseWith = withSourceSpan $ do
    Tok.symbol Tok.TWith
    _withWith <- Parsec.sepBy1 var (Tok.symbol Tok.TPeriod)
    Tok.symbol Tok.TAs
    _withAs <- term
    return $ \_withAnn -> With {..}

-- | Parse a comprehension.  These use 'Parsec.try' and should be tried before
-- parsing regular arrays/objects etc.
comprehension
    :: Tok.Token       -- ^ '(' or '{'
    -> Tok.Token       -- ^ ')' or '}'
    -> FregotParser a  -- ^ Comprehension head
    -> FregotParser (a, RuleBody SourceSpan Var)
comprehension open close phead = do
    chead <- Parsec.try $ do
        Tok.symbol open
        x <- phead
        Tok.symbol Tok.TPipe
        return x
    cbody <- query
    Tok.symbol close
    return (chead, cbody)

-- | An expression between braces separated with commas.  There may be a
-- trailing comma.  We also need a start and end token.
sepTrailing
    :: Tok.Token         -- ^ '(' or '{'
    -> Tok.Token         -- ^ ')' or '}'
    -> Tok.Token         -- ^ ',' or ';'
    -> FregotParser a    -- ^ Parser
    -> FregotParser [a]  -- ^ Result
sepTrailing open close comma p = do
    Tok.symbol open
    Parsec.choice
        [ Tok.symbol close >> return []
        , reverse <$> go []
        ]
  where
    go acc = do
        x <- p
        Parsec.choice
            [ Tok.symbol close >> return (x : acc)
            , do
                expectToken comma
                Parsec.choice
                    [ Tok.symbol close >> return (x : acc)
                    , go (x : acc)
                    ]
            ]
