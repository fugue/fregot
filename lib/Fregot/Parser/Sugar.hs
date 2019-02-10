module Fregot.Parser.Sugar
    ( expr
    ) where

import           Control.Applicative       ((<|>))
import qualified Data.Scientific           as Scientific
import           Fregot.Parser.Internal
import qualified Fregot.Parser.Token       as Tok
import           Fregot.Sources.SourceSpan
import           Fregot.Sugar
import qualified Text.Parsec               as Parsec

var :: FregotParser Var
var = Var <$> Tok.var

expr :: FregotParser (Expr SourceSpan)
expr = withSourceSpan $ do
    t <- term
    return $ \s -> Term s t

term :: FregotParser (Term SourceSpan)
term = withSourceSpan $
    (do
        v <- var
        return $ \ss -> VarT ss v) <|>
    (do
        s <- scalar
        return $ \ss -> ScalarT ss s) <|>
    (do
        a <- array
        return $ \ss -> ArrayT ss a) <|>
    (do
        o <- object
        return $ \ss -> ObjectT ss o)

scalar :: FregotParser (Scalar SourceSpan)
scalar =
    (String <$> Tok.string) <|>
    (do
        intOrFloat <- Tok.intOrFloat
        case intOrFloat of
            Left  x -> return $! Number $!  fromIntegral x
            Right x -> return $! Number $!  Scientific.fromFloatDigits x)

array :: FregotParser [Term SourceSpan]
array = commaSepTrailing Tok.TLBracket Tok.TRBracket term

object :: FregotParser (Object SourceSpan)
object =
    commaSepTrailing Tok.TLBrace Tok.TRBrace item
  where
    item = do
        k <- objectKey
        expectToken Tok.TColon
        t <- term
        return (k, t)

objectKey :: FregotParser (ObjectKey SourceSpan)
objectKey = withSourceSpan $
    (do
        s <- scalar
        return $ \ss -> ScalarK ss s)

-- | An expression between braces separated with commas.  There may be a
-- trailing comma.  We also need a start and end token.
commaSepTrailing
    :: Tok.Token         -- '(' or '{'
    -> Tok.Token         -- ')' or '}'
    -> FregotParser a    -- ^ Parser
    -> FregotParser [a]  -- ^ Result
commaSepTrailing open close p = do
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
                expectToken Tok.TComma
                Parsec.choice
                    [ Tok.symbol close >> return (x : acc)
                    , go (x : acc)
                    ]
            ]

