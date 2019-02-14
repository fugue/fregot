{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Fregot.Interpreter
    ( InterpreterM
    , Handle
    , withHandle

    , loadModule
    ) where

import           Control.Lens               ((^.))
import           Control.Lens.TH            (makeLenses)
import           Control.Monad              (foldM)
import           Control.Monad.Parachute    (ParachuteT)
import           Control.Monad.Trans        (liftIO)
import qualified Data.HashMap.Strict        as HMS
import           Data.IORef.Extended        (IORef)
import qualified Data.IORef.Extended        as IORef
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text.IO               as T
import           Fregot.Error               (Error)
import           Fregot.Interpreter.Package (Package)
import qualified Fregot.Interpreter.Package as Package
import qualified Fregot.Parser              as Parser
import qualified Fregot.PrettyPrint         as PP
import qualified Fregot.Sources             as Sources
import           Fregot.Sources.SourceSpan  (SourceSpan)
import qualified Fregot.Sugar               as Sugar
import qualified System.IO                  as IO

type InterpreterM a = ParachuteT Error IO a

data Handle = Handle
    { _sources  :: !Sources.Handle
    , _packages :: !(IORef (HMS.HashMap Sugar.PackageName Package))
    }

$(makeLenses ''Handle)

withHandle
    :: Sources.Handle
    -> (Handle -> InterpreterM a)
    -> InterpreterM a
withHandle _sources f = do
    _packages <- liftIO $ IORef.newIORef HMS.empty
    f Handle {..}

loadModule :: Handle -> FilePath -> InterpreterM ()
loadModule h path = do
    -- Read the source code and parse the module.
    input <- liftIO $ T.readFile path
    liftIO $ IORef.atomicModifyIORef_ (h ^. sources) $
        Sources.insert sourcep input
    modul <- Parser.lexAndParse Parser.parseModule sourcep input
    liftIO $ PP.hPutSemDoc IO.stdout $ PP.pretty modul

    -- Insert the module into the packages system.
    insertModule h modul
  where
    sourcep = Sources.FileInput path

-- | TODO(jaspervdj): This will require a lock if we concurrently load modules.
insertModule :: Handle -> Sugar.Module SourceSpan -> InterpreterM ()
insertModule h modul = do
    -- Lookup an existing package or create a new one if necessary.
    package0 <- fromMaybe (Package.empty pkgname) . HMS.lookup pkgname <$>
        liftIO (IORef.readIORef (h ^. packages))

    -- One by one, add the rules in the sugared module to the package.
    package1 <- foldM
        (\pkg rule -> Package.insert (modul ^. Sugar.moduleImports) rule pkg)
        package0
        (modul ^. Sugar.modulePolicy)

    -- Save the modified package.
    liftIO $ IORef.atomicModifyIORef_ (h ^. packages) $
        HMS.insert pkgname package1
  where
    pkgname = modul ^. Sugar.modulePackage
