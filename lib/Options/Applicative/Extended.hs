{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Options.Applicative.Extended
    ( module Options.Applicative
    , plainEnumOption
    , dualSwitch
    ) where

import           Data.Char           (toLower)
import           Data.List           (intercalate)
import           Options.Applicative

type PlainEnum a = (Bounded a, Enum a, Show a)

plainEnumOption
    :: forall a. (PlainEnum a) => Mod OptionFields a -> Parser a
plainEnumOption = option reader
  where
    reader :: ReadM a
    reader = eitherReader $ \txt -> case lookup txt values of
        Just x  -> Right x
        Nothing -> Left $ "unexpected " ++ txt ++ ", must be one of: " ++
            intercalate ", " (map fst values)

    values :: [(String, a)]
    values = [(map toLower (show x), x) | x <- [minBound .. maxBound]]

dualSwitch
    :: String                     -- ^ Option xxx
    -> Mod FlagFields Bool  -- ^ Info for xxx
    -> Mod FlagFields Bool  -- ^ Info for --no-xxx
    -> Parser (Maybe Bool)
dualSwitch name yesMod noMod = combine
    <$> switch (long name            <> yesMod <> hidden)
    <*> switch (long ("no-" ++ name) <> noMod  <> hidden)
  where
    combine False False = Nothing
    combine True  False = Just True
    combine False True  = Just False
    combine _     _     = Nothing
