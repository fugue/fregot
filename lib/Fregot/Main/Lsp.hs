module Fregot.Main.Lsp
    ( main
    ) where

import qualified Data.Default                 as Default
import qualified Language.Haskell.LSP.Control as LSP
import qualified Language.Haskell.LSP.Core    as LSP
import           System.Exit                  (ExitCode (..))

main :: IO ExitCode
main = do
    code <- LSP.run initializeCallbacks handlers options Nothing
    pure $ case code of
        0 -> ExitSuccess
        n -> ExitFailure n

initializeCallbacks :: LSP.InitializeCallbacks Int
initializeCallbacks = LSP.InitializeCallbacks
    { LSP.onInitialConfiguration = undefined
    , LSP.onConfigurationChange  = undefined
    , LSP.onStartup              = undefined
    }

handlers :: LSP.Handlers
handlers = Default.def

options :: LSP.Options
options = Default.def
