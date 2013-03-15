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
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.Autohead
#if DEVELOPMENT
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
#else
import Network.Wai.Middleware.RequestLogger (logStdout)
#endif
import Data.Maybe (fromMaybe)
import Data.IORef (newIORef, writeIORef)
import System.Process (runProcess, waitForProcess)
import Yesod.Static (Static (Static))
import Network.Wai.Application.Static (defaultFileServerSettings)
import Control.Monad (unless, forever)
import Filesystem (isDirectory)
import System.Process (rawSystem)
import System.Exit (ExitCode (ExitSuccess), exitWith)
import Control.Concurrent (forkIO, threadDelay)

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
getApplication :: AppConfig DefaultEnv Extra -> IO Application
getApplication conf = do
    exists <- isDirectory "content"
    unless exists $ do
        putStrLn "Cloning content"
        ec <- rawSystem "git" ["clone", "https://github.com/yesodweb/yesodweb.com-content.git", "content"]
        unless (ec == ExitSuccess) $ do
            putStrLn "git clone failed, exiting"
            exitWith ec

    s <- staticSite
    let assets = Static $ defaultFileServerSettings "content/static"

    mblog <- loadBlog
    iblog <- newIORef $ fromMaybe (error "Invalid posts.yaml") mblog
    ibook <- loadBook >>= newIORef
    iauthors <- loadAuthors >>= newIORef

    let foundation = YesodWeb conf s assets iblog ibook iauthors
    app <- toWaiApp foundation
    return $ gzip def
           $ autohead
           $ logWare
             app
  where
#ifdef DEVELOPMENT
    logWare = logStdoutDev
#else
    logWare = logStdout
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
    liftIO $ loadAuthors >>= writeIORef (ywAuthors yw)
