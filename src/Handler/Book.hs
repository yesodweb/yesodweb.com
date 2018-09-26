module Handler.Book
    ( getBookHomeR
    , getChapterR
    , getBookImageR
    ) where

import Import hiding (fix)
import qualified Data.Text as T
import Book
import qualified Data.Map as Map
import Text.XML
import Network.HTTP.Types (status301)
import Book.Routes
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.HTML.DOM
import Yesod.Form.Jquery (urlJqueryJs)
import RIO.FilePath
import RIO.Directory

getBookHomeR :: SubHandlerFor BookSub YesodWeb Html
getBookHomeR = do
    bs <- getSubYesod
    let ibook = bsBook bs
    Book parts _ <- readIORef ibook >>= either throwIO pure
    toMaster <- getRouteToParent
    liftHandler $ defaultLayout $ do
        setTitle $ bsTitle bs
        $(widgetFile "book")
        $(widgetFile "booklist")

getChapterR :: Text -> SubHandlerFor BookSub YesodWeb Html
getChapterR slug = do
    bs <- getSubYesod
    let ibook = bsBook bs
    Book parts m <- readIORef ibook >>= either throwIO pure
    chapter <- maybe notFound return $ Map.lookup slug m
    toMaster <- getRouteToParent

    mraw <- lookupGetParam "raw"
    case mraw of
        Nothing -> liftHandler $ defaultLayout $ do
            setTitle $ mconcat
                [ toHtml $ chapterTitle chapter
                , " :: "
                , bsTitle bs
                ]
            getYesod >>= addScriptEither . urlJqueryJs
            $(widgetFile "chapter")
            $(widgetFile "booklist")
        Just raw -> return
            [shamlet|
                $doctype 5
                <html>
                    <head>
                        <title>#{chapterTitle chapter}
                    <body>
                        <article>#{sohFixes raw $ chapterHtml chapter}
            |]
  where
    sohFixes "soh" x =
        mconcat $ map (toHtml . fixSOH) nodes
      where
        Document _ (Element _ _ nodes) _ = Text.HTML.DOM.parseLBS $ renderHtml $ H.div x
    sohFixes _ x = x

fixSOH :: Node -> Node
fixSOH (NodeElement (Element "code" attrs [NodeContent t])) =
    NodeElement $ Element "code" attrs [NodeContent $ T.intercalate "\n" $ map go $ T.splitOn "\n" t]
  where
    go = T.replace "warp 3000" "warpEnv"
fixSOH (NodeElement (Element "img" attrs nodes)) =
    NodeElement $ Element "img" attrs' $ map fixSOH nodes
  where
    attrs' = Map.fromList $ map fix $ Map.toList attrs
    fix ("src", t) | "image/" `T.isPrefixOf` t = ("src", T.append "http://www.yesodweb.com/book/" t)
    fix x = x
fixSOH (NodeElement (Element name attrs nodes)) =
    NodeElement $ Element name attrs $ map fixSOH nodes
fixSOH n = n

getBookImageR :: Text -> SubHandlerFor BookSub YesodWeb ()
getBookImageR name
    | name' == name'' = do
        bs <- getSubYesod
        let fp1 = bsRoot bs </> "images" </> name' <.> "png"
            fp2 = bsRoot bs </> "asciidoc" </> "images" </> name' <.> "png"
        fp <- do
            x <- doesFileExist fp1
            return $ if x then fp1 else fp2
        sendFile "image/png" fp
    | otherwise = do
        tp <- getRouteToParent
        redirectWith status301 $ tp $ BookImageR $ T.pack name'
  where
    name' = takeBaseName name''
    name'' = T.unpack name
