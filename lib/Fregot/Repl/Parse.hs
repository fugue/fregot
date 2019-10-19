-- | Utility parsing module.
module Fregot.Repl.Parse
    ( parseRuleOrQuery
    ) where

import           Control.Lens              (review, (^.))
import           Control.Monad.Parachute   (ParachuteT)
import qualified Control.Monad.Parachute   as Parachute
import           Data.Maybe                (isNothing)
import qualified Data.Text                 as T
import           Fregot.Error              (Error)
import qualified Fregot.Parser             as Parser
import qualified Fregot.Sources            as Sources
import           Fregot.Sources.SourceSpan (SourceSpan)
import           Fregot.Sugar

parseRuleOrQuery
    :: Monad m
    => Sources.SourcePointer -> T.Text
    -> ParachuteT Error m (Either (Rule SourceSpan Var) (Query SourceSpan Var))
parseRuleOrQuery sourcep input = do
    ruleOrQuery <- Parachute.catch
        (Left <$> Parser.lexAndParse Parser.rule sourcep input)
        (\_ -> Right <$> Parser.lexAndParse Parser.query sourcep input)

    case ruleOrQuery of
        Left r | Just q <- ruleToQuery r -> return (Right q)
        _                                -> return ruleOrQuery

termToQuery :: Term a n -> Query a n
termToQuery = review $ literalFromQuery . exprFromLiteral . termFromExpr

ruleToQuery :: Rule a Var -> Maybe (Query a Var)
ruleToQuery r
    | null (r ^. ruleBodies)
    , isNothing (r ^. ruleHead . ruleValue) =
        case (r ^. ruleHead . ruleIndex, r ^. ruleHead . ruleArgs) of
            (Just idx, _) -> Just $ termToQuery $ RefT a a
                (r ^. ruleHead . ruleName) [RefBrackArg (TermE a idx)]
            (_, Just args) -> Just $ termToQuery $
                CallT a [r ^. ruleHead . ruleName]
                (map (review termFromExpr) args)
            _ -> Just $ termToQuery $
                VarT a (r ^. ruleHead . ruleName)

    | otherwise = Nothing
  where
    a = r ^. ruleHead . ruleAnn
