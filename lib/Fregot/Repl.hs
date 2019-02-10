module Fregot.Repl
    ( Handle
    , withHandle
    , run
    ) where

import           Control.Monad.Parachute
import           Control.Monad.Trans      (liftIO)
import qualified Data.List                as L
import qualified Data.Text                as T
import qualified Fregot.Error             as Error
import qualified Fregot.Parser.Internal   as Parser
import qualified Fregot.Parser.Sugar      as Parser
import qualified Fregot.PrettyPrint       as PP
import qualified Fregot.Sources           as Sources
import qualified System.Console.Haskeline as Hl
import qualified System.IO.Extended       as IO

data Handle = Handle

withHandle :: (Handle -> IO a) -> IO a
withHandle = ($ Handle)

processInput :: Handle -> T.Text -> IO ()
processInput _ input = do
    (errors, mbExpr) <- runParachuteT $
        Parser.lexAndParse Parser.expr sourcep input
    Error.hPutErrors IO.stderr sources Error.TextFmt errors
    case mbExpr of
        Just expr -> PP.hPutSemDoc IO.stdout $ PP.pretty expr
        Nothing   -> return ()
  where
    sourcep = Sources.ReplInput 0 input
    sources = Sources.insert sourcep input Sources.empty

run :: Handle -> IO ()
run h = do
    IO.hPutStrLn IO.stderr $ L.intersperse ' ' "Fugue REGO Toolkit"
    Hl.runInputT Hl.defaultSettings loop
  where
    loop :: Hl.InputT IO ()
    loop = do
        minput <- Hl.getInputLine "% "
        case minput of
            Nothing -> return ()
            Just "quit" -> return ()
            Just input -> do
                liftIO $ processInput h (T.pack input)
                loop
