module Handler.Book
    ( getBookR
    , getChapterR
    , getBookImageR
    ) where

import Import
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as F
import Settings (bookRoot)
import Book
import qualified Data.Map as Map
import Text.XML
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Network.HTTP.Types (status301)
import Data.IORef (readIORef)

getBookR :: Handler RepHtml
getBookR = do
    ibook <- ywBook <$> getYesod
    Book parts _ <- liftIO $ readIORef ibook
    defaultLayout $ do
        setTitle "Yesod Web Framework Book"
        $(widgetFile "book")
        $(widgetFile "booklist")

getChapterR :: Text -> Handler RepHtml
getChapterR slug = do
    ibook <- ywBook <$> getYesod
    Book parts m <- liftIO $ readIORef ibook
    chapter <- maybe notFound return $ Map.lookup slug m
    defaultLayout $ do
        setTitle $ toHtml $ chapterTitle chapter
        [whamlet|<section .why><article>#{chapterHtml chapter}|]
        $(widgetFile "booklist")

getBookImageR :: Text -> Handler ()
getBookImageR name
    | name' == name'' =
        sendFile "image/png" $ F.encodeString $
            bookRoot F.</> "images" F.</> name' F.<.> "png"
    | otherwise = redirectWith status301 $ BookImageR $ either id id $ F.toText name'
  where
    name' = F.basename name''
    name'' = F.fromText name
