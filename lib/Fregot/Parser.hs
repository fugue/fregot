{-|
Copyright   : (c) 2020 Fugue, Inc.
License     : Apache License, version 2.0
Maintainer  : jasper@fugue.co
Stability   : experimental
Portability : POSIX
-}
module Fregot.Parser
    ( FregotParser
    , lexAndParse

    , module Fregot.Parser.Sugar
    ) where

import           Fregot.Parser.Internal
import           Fregot.Parser.Sugar
