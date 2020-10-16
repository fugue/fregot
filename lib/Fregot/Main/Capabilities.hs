{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
module Fregot.Main.Capabilities
    ( Options
    , parseOptions

    , main
    ) where

import qualified Data.Aeson.Extended        as A
import qualified Data.ByteString.Lazy       as BL
import           Fregot.Builtins            (defaultBuiltins)
import           Fregot.Capabilities
import           Fregot.Main.GlobalOptions
import qualified Options.Applicative        as OA
import           System.Exit                (ExitCode (..))
import qualified System.IO                  as IO

data Options = Options deriving (Show)

parseOptions :: OA.Parser Options
parseOptions = pure Options

main :: GlobalOptions -> Options -> IO ExitCode
main _gopts _opts = do
    BL.hPutStr IO.stdout . A.encodePretty $ renderCapabilities defaultBuiltins
    return ExitSuccess
