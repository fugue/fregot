-- | Utility parsing module.
module Fregot.Repl.Parse
    ( parseRuleOrExpr
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

parseRuleOrExpr
    :: Monad m
    => Sources.SourcePointer -> T.Text
    -> ParachuteT Error m (Either (Rule SourceSpan Var) (Expr SourceSpan Var))
parseRuleOrExpr sourcep input = do
    ruleOrExpr <- Parachute.catch
        (Left <$> Parser.lexAndParse Parser.rule sourcep input)
        (\_ -> Right <$> Parser.lexAndParse Parser.expr sourcep input)

    case ruleOrExpr of
        Left r | Just e <- ruleToExpr r -> return (Right e)
        _                               -> return ruleOrExpr
  where
    ruleToExpr r
        | null (r ^. ruleBodies)
        , isNothing (r ^. ruleHead . ruleValue) =
            case (r ^. ruleHead . ruleIndex, r ^. ruleHead . ruleArgs) of
                (Just idx, _) -> Just $ TermE a $
                    RefT a a
                        (r ^. ruleHead . ruleName) [RefBrackArg (TermE a idx)]
                (_, Just args) -> Just $ TermE a $
                    CallT a [r ^. ruleHead . ruleName]
                    (map (review termFromExpr) args)
                _ -> Just $ TermE a $
                    VarT a (r ^. ruleHead . ruleName)

        | otherwise = Nothing
      where
        a = r ^. ruleHead . ruleAnn
