module Handler.Book
    ( getBookHomeR
    , getChapterR
    , getBookImageR
    ) where

import Import
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as F
import Book
import qualified Data.Map as Map
import Text.XML
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Network.HTTP.Types (status301)
import Data.IORef (readIORef)
import Book.Routes

getBookHomeR :: HandlerT BookSub Handler RepHtml
getBookHomeR = do
    bs <- getYesod
    let ibook = bsBook bs
    Book parts _ <- liftIO $ readIORef ibook
    toMaster <- getRouteToParent
    lift $ defaultLayout $ do
        setTitle $ bsTitle bs
        $(widgetFile "book")
        $(widgetFile "booklist")

getChapterR :: Text -> HandlerT BookSub Handler RepHtml
getChapterR slug = do
    bs <- getYesod
    let ibook = bsBook bs
    Book parts m <- liftIO $ readIORef ibook
    chapter <- maybe notFound return $ Map.lookup slug m
    toMaster <- getRouteToParent
    lift $ defaultLayout $ do
        setTitle $ mconcat
            [ toHtml $ chapterTitle chapter
            , " :: "
            , bsTitle bs
            ]
        $(widgetFile "chapter")
        $(widgetFile "booklist")

getBookImageR :: Text -> HandlerT BookSub Handler ()
getBookImageR name
    | name' == name'' = do
        bs <- getYesod
        sendFile "image/png" $ F.encodeString $
            bsRoot bs F.</> "images" F.</> name' F.<.> "png"
    | otherwise = redirectWith status301 $ BookImageR $ either id id $ F.toText name'
  where
    name' = F.basename name''
    name'' = F.fromText name
