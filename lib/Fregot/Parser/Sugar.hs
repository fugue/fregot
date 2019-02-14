{-# LANGUAGE RecordWildCards #-}
module Fregot.Parser.Sugar
    ( parseModule
    , rule
    , expr
    ) where

import           Control.Applicative       ((<|>))
import           Data.Functor              (($>))
import qualified Data.Scientific           as Scientific
import           Fregot.Parser.Internal
import qualified Fregot.Parser.Token       as Tok
import           Fregot.Sources.SourceSpan
import           Fregot.Sugar
import           Prelude                   hiding (head)
import qualified Text.Parsec               as Parsec
import qualified Text.Parsec.Expr          as Parsec

parsePackageName :: FregotParser PackageName
parsePackageName =
    PackageName <$> Parsec.sepBy1 Tok.var (Tok.symbol Tok.TPeriod)

parseModule :: FregotParser (Module SourceSpan)
parseModule = Module
    <$> parseModuleHead
    <*> Parsec.many parseModuleImport
    <*> Parsec.many rule

parseModuleHead :: FregotParser PackageName
parseModuleHead = Tok.symbol Tok.TPackage *> parsePackageName

parseModuleImport :: FregotParser (Import SourceSpan)
parseModuleImport = withSourceSpan $ do
    Tok.symbol Tok.TImport
    _importPackage <- parsePackageName
    _importAs <- Parsec.optionMaybe $ do
        Tok.symbol Tok.TAs
        var
    return $ \_importAnn -> Import {..}

var :: FregotParser Var
var = Var <$> Tok.var

rule :: FregotParser (Rule SourceSpan)
rule = Rule <$> parseRuleHead <*> Parsec.option [] parseRuleBody

parseRuleHead :: FregotParser (RuleHead SourceSpan)
parseRuleHead = withSourceSpan $ do
    _ruleName <- var
    _ruleIndex <- Parsec.optionMaybe $ do
        Tok.symbol Tok.TLBracket
        t <- term
        expectToken Tok.TRBracket
        return t
    _ruleValue <- Parsec.optionMaybe $ do
        Tok.symbol Tok.TAssign
        term
    return $ \_ruleAnn -> RuleHead {..}

parseRuleBody :: FregotParser [Literal SourceSpan]
parseRuleBody = do
    Tok.symbol Tok.TLBrace
    lits <- blockOrSemi literal
    Tok.symbol Tok.TRBrace
    return lits

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

literal :: FregotParser (Literal SourceSpan)
literal = do
    _literalNegation <- Parsec.option False $ Tok.symbol Tok.TNot $> True
    _literalExpr <- unificationExpr
    _literalWith <- Parsec.many parseWith
    return Literal {..}

unificationExpr :: FregotParser (Expr SourceSpan)
unificationExpr = toUnification <$> expr
  where
    toUnification (BinOpE a x AssignO y) = UnifyE a x y
    toUnification e                      = e

expr :: FregotParser (Expr SourceSpan)
expr = Parsec.buildExpressionParser
    [ [ binary Tok.TTimes  TimesO  Parsec.AssocLeft
      , binary Tok.TDivide DivideO Parsec.AssocLeft
      ]

    , [ binary Tok.TPlus  PlusO  Parsec.AssocLeft
      , binary Tok.TMinus MinusO Parsec.AssocLeft
      ]

    , [ binary Tok.TLessThan           LessThanO           Parsec.AssocLeft
      , binary Tok.TLessThanOrEqual    LessThanOrEqualO    Parsec.AssocLeft
      , binary Tok.TGreaterThan        GreaterThanO        Parsec.AssocLeft
      , binary Tok.TGreaterThanOrEqual GreaterThanOrEqualO Parsec.AssocLeft
      ]

    , [ binary Tok.TEqual    EqualO    Parsec.AssocLeft
      , binary Tok.TNotEqual NotEqualO Parsec.AssocLeft
      ]

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
            BinOpE (unsafeMergeSourceSpan (exprAnn x) (exprAnn y)) x op y)

term :: FregotParser (Term SourceSpan)
term = withSourceSpan $
    (do
        (v, vss) <- withSourceSpan $ var >>= \v -> return $ \vss -> (v, vss)
        as       <- Parsec.many refArg
        return $ \ss -> if null as then VarT ss v else RefT ss vss v as) <|>
    (do
        s <- scalar
        return $ \ss -> ScalarT ss s) <|>
    (do
        a <- array
        return $ \ss -> ArrayT ss a) <|>
    (do
        o <- object
        return $ \ss -> ObjectT ss o)

refArg :: FregotParser (RefArg SourceSpan)
refArg =
    (do
        Tok.symbol Tok.TLBracket
        t <- term
        expectToken Tok.TRBracket
        return $ RefBrackArg t) <|>
    (withSourceSpan $ do
        Tok.symbol Tok.TPeriod
        v <- var
        return $ \ss -> RefDotArg ss v)

scalar :: FregotParser (Scalar SourceSpan)
scalar =
    (String <$> Tok.string) <|>
    (do
        intOrFloat <- Tok.intOrFloat
        case intOrFloat of
            Left  x -> return $! Number $! fromIntegral x
            Right x -> return $! Number $! Scientific.fromFloatDigits x) <|>
    (pure (Bool True) <* Tok.symbol Tok.TTrue) <|>
    (pure (Bool False) <* Tok.symbol Tok.TFalse) <|>
    (pure Null <* Tok.symbol Tok.TNull)

array :: FregotParser [Expr SourceSpan]
array = sepTrailing Tok.TLBracket Tok.TRBracket Tok.TComma expr

object :: FregotParser (Object SourceSpan)
object =
    sepTrailing Tok.TLBrace Tok.TRBrace Tok.TComma item
  where
    item = do
        k <- objectKey
        expectToken Tok.TColon
        e <- expr
        return (k, e)

objectKey :: FregotParser (ObjectKey SourceSpan)
objectKey = withSourceSpan $
    (do
        s <- scalar
        return $ \ss -> ScalarK ss s)

parseWith :: FregotParser (With SourceSpan)
parseWith = do
    Tok.symbol Tok.TWith
    _withWith <- term
    Tok.symbol Tok.TAs
    _withAs <- term
    return With {..}

-- | An expression between braces separated with commas.  There may be a
-- trailing comma.  We also need a start and end token.
sepTrailing
    :: Tok.Token         -- '(' or '{'
    -> Tok.Token         -- ')' or '}'
    -> Tok.Token         -- ',' or ';'
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

