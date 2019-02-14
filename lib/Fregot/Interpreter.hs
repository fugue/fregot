{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Fregot.Interpreter
    ( InterpreterM
    , Handle
    , withHandle

    , loadModule
    ) where

import           Control.Lens            ((^.))
import           Control.Lens.TH         (makeLenses)
import           Control.Monad.Parachute (ParachuteT)
import           Control.Monad.Trans     (liftIO)
import qualified Data.IORef.Extended     as IORef
import qualified Data.Text.IO            as T
import           Fregot.Error            (Error)
import qualified Fregot.Parser           as Parser
import qualified Fregot.PrettyPrint      as PP
import qualified Fregot.Sources          as Sources
import qualified System.IO               as IO

type InterpreterM a = ParachuteT Error IO a

data Handle = Handle
    { _sources :: !Sources.Handle
    }

$(makeLenses ''Handle)

withHandle
    :: Sources.Handle
    -> (Handle -> InterpreterM a)
    -> InterpreterM a
withHandle _sources f = do
    f Handle {..}

loadModule :: Handle -> FilePath -> InterpreterM ()
loadModule h path = do
    input <- liftIO $ T.readFile path
    liftIO $ IORef.atomicModifyIORef_ (h ^. sources) $
        Sources.insert sourcep input
    modul <- Parser.lexAndParse Parser.parseModule sourcep input
    liftIO $ PP.hPutSemDoc IO.stdout $ PP.pretty modul
  where
    sourcep = Sources.FileInput path
