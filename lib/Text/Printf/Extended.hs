-- | Works around two problems with 'Text.Printf.printf':
--
-- * It cannot take non-statically known number of arguments by default.
-- * Errors are not returned safely.
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE Rank2Types                #-}
module Text.Printf.Extended
    ( module Text.Printf

    , Some (..)
    , sprintf
    ) where

import           Control.Exception (ErrorCall (..), catch)
import           GHC.Exts          (Constraint)
import           System.IO.Unsafe  (unsafePerformIO)
import           Text.Printf

data Some (c :: * -> Constraint) = forall a. c a => Some a
data Any  (c :: * -> Constraint) = Any (forall a. c a => a)

dynprintf :: PrintfType r => String -> [Some PrintfArg] -> r
dynprintf format = go (Any (printf format))
  where
    go :: PrintfType r => Any PrintfType -> [Some PrintfArg] -> r
    go (Any x) []            = x
    go (Any f) (Some x : xs) = go (Any (f x)) xs

errorCallToEither :: String -> Either String String
errorCallToEither result = unsafePerformIO $ catch
    (length result `seq` return (Right result))
    (\(ErrorCall err) -> return (Left err))

sprintf :: String -> [Some PrintfArg] -> Either String String
sprintf format = errorCallToEither . dynprintf format
