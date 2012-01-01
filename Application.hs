{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( withYesodWeb
    , withDevelAppPort
    ) where

import Import
import Settings
import Settings.StaticFiles (staticSite)
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Data.Dynamic (Dynamic, toDyn)
#if DEVELOPMENT
import Yesod.Logger (Logger, logBS, flushLogger)
import Network.Wai.Middleware.RequestLogger (logHandleDev)
#else
import Yesod.Logger (Logger)
import Network.Wai.Middleware.RequestLogger (logStdout)
#endif
import qualified Database.Persist.Store
import Database.Persist.GenericSql (runMigration)

-- Import all relevant handler modules here.
import Handler.Root

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "YesodWeb" resourcesYesodWeb

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withYesodWeb :: AppConfig DefaultEnv Extra -> Logger -> (Application -> IO ()) -> IO ()
withYesodWeb conf logger f = do
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.Store.loadConfig
    Database.Persist.Store.withPool (dbconf :: Settings.PersistConfig) $ \p -> do
        Database.Persist.Store.runPool dbconf (runMigration migrateAll) p
        let h = YesodWeb conf logger s p
        defaultRunner (f . logWare) h
  where
#ifdef DEVELOPMENT
    logWare = logHandleDev (\msg -> logBS logger msg >> flushLogger logger)
#else
    logWare = logStdout
#endif

-- for yesod devel
withDevelAppPort :: Dynamic
withDevelAppPort =
    toDyn $ defaultDevelAppWith loader  withYesodWeb
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
