{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Application
    ( getApplication
    ) where

import           Book.Routes
import           ClassyPrelude                        (print, putStrLn)
import           Control.Concurrent                   (forkIO)
import qualified Data.Text                            as T
import           RIO.FilePath
import           RIO.Directory
import           Import
import           Network.Wai.Application.Static       (defaultFileServerSettings)
import           Network.Wai.Middleware.Autohead
import           Network.Wai.Middleware.Gzip
import           Network.Wai.Middleware.RequestLogger
import           System.Environment                   (getEnvironment)
import           System.Exit                          (ExitCode (ExitSuccess),
                                                       exitWith)
import           System.Process                       (rawSystem, runProcess,
                                                       waitForProcess)
import           Yesod.Default.Config
import           Yesod.Default.Handlers
import           Yesod.Static                         (Static (Static))

-- Import all relevant handler modules here.
import           Handler.Blog
import           Handler.Book
import           Handler.Page
import           Handler.Root
import           Handler.Wiki

instance YesodSubDispatch BookSub YesodWeb where
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
        exists <- doesDirectoryExist dir
        unless exists $ do
            putStrLn $ T.pack $ "Cloning " <> dir
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
    booksub12 <- mkBookSub "Yesod Web Framework Book- Version 1.2" "Note: You are looking at version 1.2 of the book, which is two versions behind" dir12
    booksub11 <- mkBookSub "Yesod Web Framework Book- Version 1.1" "Note: You are looking at version 1.1 of the book, which is three versions behind" dir11
    booksub14 <- mkBookSub "Yesod Web Framework Book- Version 1.4" "Note: You are looking at version 1.4 of the book, which is one version behind" dir14
    booksub16 <- mkBookSub "Yesod Web Framework Book- Version 1.6" "Note: The book has not yet been fully updated to version 1.6, there may still be some out of date information!" dir16
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
            , getBook16 = booksub16
            }
    app <- toWaiAppPlain foundation
    logWare <- mkLogWare

    -- Reload git every 5 minutes, due to lack of propagation of webhooks in a
    -- horizontally scaled situation
    void $ forkIO $ forever $ do
        threadDelay 300000000
        handleAny print $ pullGitBranches foundation

    return $ gzip def
           $ autohead
           $ logWare
             app
  where
    mkLogWare = mkRequestLogger def
        { outputFormat = Apache FromHeader
        }

mkBookSub :: Html -> Text -> FilePath -> IO BookSub
mkBookSub title warning root' = do
    Just branch <- return $ lookup root' branches
    let root = root' </> "book"
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

dir12, dir11, dir14, dir16 :: FilePath
dir12 = "content-1.2"
dir11 = "content-1.1"
dir14 = "content-1.4"
dir16 = "content"

branches :: [(FilePath, String)]
branches =
    [ (dir12, "version1.2")
    , (dir11, "version1.1")
    , (dir14, "version1.4")
    , (dir16, "master")
    ]

postReloadR :: Handler ()
postReloadR = do
    yw <- getYesod
    void $ liftIO $ forkIO $ pullGitBranches yw

pullGitBranches :: YesodWeb -> IO ()
pullGitBranches yw = do
    forM_ branches $ \(dir, branch) -> do
        let run x y = void $ runProcess x y (Just dir) Nothing Nothing Nothing Nothing >>= waitForProcess
        run "git" ["fetch"]
        run "git" ["checkout", "origin/" ++ branch]
    eblog <- loadBlog
    case eblog of
        Left e -> print e
        Right blog -> writeIORef (ywBlog yw) blog
    bsReload $ getBook16 yw
    bsReload $ getBook14 yw
    bsReload $ getBook12 yw
    bsReload $ getBook11 yw
    loadAuthors >>= writeIORef (ywAuthors yw)
