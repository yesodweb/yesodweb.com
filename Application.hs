{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Application
    ( getApplication
    ) where

import Import
import Yesod.Default.Config
import Yesod.Default.Handlers
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.Autohead
import Book.Routes
import Network.Wai.Middleware.RequestLogger
import Data.Maybe (fromMaybe)
import Data.IORef (newIORef, writeIORef)
import System.Process (runProcess, waitForProcess)
import Yesod.Static (Static (Static))
import Network.Wai.Application.Static (defaultFileServerSettings)
import Control.Monad (unless, forM_)
import Filesystem (isDirectory)
import System.Process (rawSystem)
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.Environment (getEnvironment)
import Control.Concurrent (forkIO)
import qualified Filesystem.Path.CurrentOS as F
import qualified Data.Text as T

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Wiki
import Handler.Page
import Handler.Blog
import Handler.Book

instance YesodSubDispatch BookSub (HandlerT YesodWeb IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesBookSub)

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
    env <- getEnvironment
    forM_ branches $ \(dir, branch) -> do
        exists <- isDirectory $ F.decodeString dir
        unless exists $ do
            putStrLn $ "Cloning " ++ dir
            ec <- rawSystem "git"
                [ "clone"
                , "-b"
                , branch
                , fromMaybe "https://github.com/yesodweb/yesodweb.com-content.git"
                  $ lookup "CONTENT_REPO" env
                , dir
                ]
            unless (ec == ExitSuccess) $ do
                putStrLn "git clone failed, exiting"
                exitWith ec

    s <- staticSite
    let assets = Static $ defaultFileServerSettings "content/static"

    eblog <- loadBlog
    blog <-
        case eblog of
            Left e -> do
                print e
                return $ error $ "Invalid posts.yaml: " ++ show e
            Right b -> return b
    iblog <- newIORef blog
    booksub12 <- mkBookSub "Yesod Web Framework Book- Version 1.2" "" $ F.decodeString dirCurrent
    booksub11 <- mkBookSub "Yesod Web Framework Book- Version 1.1" "Note: You are looking at version 1.1 of the book, which is one version behind" $ F.decodeString dir11
    booksub14 <- mkBookSub "Yesod Web Framework Book- Version 1.4" "Note: This is a preview release for the upcoming version of Yesod" $ F.decodeString dir14
    iauthors <- loadAuthors >>= newIORef

    let foundation = YesodWeb
            { settings = conf
            , getStatic = s
            , getAssets = assets
            , ywBlog = iblog
            , ywAuthors = iauthors
            , getBook12 = booksub12
            , getBook11 = booksub11
            , getBook14 = booksub14
            }
    app <- toWaiAppPlain foundation
    logWare <- mkLogWare
    return $ gzip def
           $ autohead
           $ logWare
             app
  where
    mkLogWare = mkRequestLogger def
        { outputFormat = Apache FromHeader
        }

mkBookSub :: Html -> Text -> F.FilePath -> IO BookSub
mkBookSub title warning root' = do
    Just branch <- return $ lookup (F.encodeString root') branches
    let root = root' F.</> "book"
    ibook <- newIORef $ error "Still loading"
    _ <- forkIO $ loadBook root >>= writeIORef ibook
    return BookSub
        { bsRoot = root
        , bsBook = ibook
        , bsReload = loadBook root >>= writeIORef ibook
        , bsTitle = title
        , bsWarning = if T.null warning then Nothing else Just (toHtml warning)
        , bsBranch = T.pack branch
        }

dirCurrent, dir11, dir14 :: FilePath
dirCurrent = "content"
dir11 = "content-1.1"
dir14 = "content-1.4"

branches :: [(FilePath, String)]
branches =
    [ (dirCurrent, "master")
    , (dir11, "version1.1")
    , (dir14, "version1.4")
    ]

postReloadR :: Handler ()
postReloadR = do
    yw <- getYesod
    _ <- liftIO $ forkIO $ do
        forM_ branches $ \(dir, branch) -> do
            let run x y = runProcess x y (Just dir) Nothing Nothing Nothing Nothing >>= waitForProcess >> return ()
            run "git" ["fetch"]
            run "git" ["checkout", "origin/" ++ branch]
        eblog <- loadBlog
        case eblog of
            Left e -> print e
            Right blog -> writeIORef (ywBlog yw) blog
        bsReload $ getBook12 yw
        bsReload $ getBook11 yw
        loadAuthors >>= writeIORef (ywAuthors yw)
    return ()
