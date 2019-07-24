module Data.Aeson.TH.Extended
    ( module Data.Aeson.TH
    , fregotOptions
    ) where

import           Data.Aeson.TH
import           Data.Char     (isLower, isUpper, toLower)

-- | Fregot options for JSON deriving.
fregotOptions :: Options
fregotOptions = defaultOptions
    { fieldLabelModifier     = dropPrefix
    , constructorTagModifier = smartDownCase
    }

dropPrefix :: String -> String
dropPrefix str = case break isUpper (dropUnderscore str) of
    (_, (y : ys)) -> toLower y : ys
    (lowers, [])  -> lowers
  where
    dropUnderscore = dropWhile (== '_')

-- | Converts 'IOSystem' to 'ioSystem', 'FooBar' into 'fooBar', 'HTTPCall' into
-- 'httpCall' and so on.
smartDownCase :: String -> String
smartDownCase str = case break isLower str of
    (xs, []) -> map toLower xs
    (xs, ys) -> case length xs of
        0 -> ys
        1 -> map toLower xs ++ ys
        _ -> map toLower (init xs) ++ [last xs] ++ ys
