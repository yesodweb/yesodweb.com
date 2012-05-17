{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplication
    , getApplicationDev
    ) where

import Import
import Settings
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
#if DEVELOPMENT
import Yesod.Logger (Logger, logBS)
import Network.Wai.Middleware.RequestLogger (logCallbackDev)
#else
import Yesod.Logger (Logger, logBS, toProduction)
import Network.Wai.Middleware.RequestLogger (logCallback)
#endif
import Data.Maybe (fromMaybe)
import Data.IORef (newIORef, writeIORef)
import System.Process (runProcess, waitForProcess)
import Yesod.Static (Static (Static))
import Network.Wai.Application.Static (defaultFileServerSettings, ssFolder, fileSystemLookup)
import Control.Monad (unless, forever)
import Filesystem (isDirectory)
import System.Process (rawSystem)
import System.Exit (ExitCode (ExitSuccess), exitWith)
import Control.Concurrent (forkIO, threadDelay)
import Yesod.Logger (flushLogger)

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Wiki
import Handler.Page
import Handler.Blog
import Handler.Book

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "YesodWeb" resourcesYesodWeb

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
getApplication :: AppConfig DefaultEnv Extra -> Logger -> IO Application
getApplication conf logger = do
    _ <- forkIO $ forever $ do
        threadDelay $ 1000 * 1000
        flushLogger logger

    exists <- isDirectory "content"
    unless exists $ do
        ec <- rawSystem "git" ["clone", "https://github.com/yesodweb/yesodweb.com-content.git", "content"]
        unless (ec == ExitSuccess) $ exitWith ec

    s <- staticSite
    let assets = Static defaultFileServerSettings { ssFolder = fileSystemLookup "content/static" }

    mblog <- loadBlog
    iblog <- newIORef $ fromMaybe (error "Invalid posts.yaml") mblog
    ibook <- loadBook >>= newIORef

    let foundation = YesodWeb conf setLogger s assets iblog ibook
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
#ifdef DEVELOPMENT
    logWare = logCallbackDev (logBS setLogger)
    setLogger = logger
#else
    setLogger = toProduction logger -- by default the logger is set for development
    logWare = logCallback (logBS setLogger)
#endif

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader getApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }

postReloadR :: Handler ()
postReloadR = do
    let run x y = liftIO $ runProcess x y (Just "content") Nothing Nothing Nothing Nothing >>= waitForProcess >> return ()
    run "git" ["fetch"]
    run "git" ["checkout", "origin/master"]
    yw <- getYesod
    mblog <- liftIO loadBlog
    case mblog of
        Nothing -> return ()
        Just blog -> liftIO $ writeIORef (ywBlog yw) blog
    liftIO $ loadBook >>= writeIORef (ywBook yw)
