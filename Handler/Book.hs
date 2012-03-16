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
import Text.XML.Xml2Html ()
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

getChapterR :: Text -> Handler RepHtml
getChapterR slug = do
    ibook <- ywBook <$> getYesod
    Book _ m <- liftIO $ readIORef ibook
    chapter <- maybe notFound return $ Map.lookup slug m
    Document _ (Element _ _ ns) _ <- liftIO $
        Text.XML.readFile def $ chapterPath chapter
    r <- getUrlRender
    defaultLayout $ do
        setTitle $ toHtml $ chapterTitle chapter
        let content = mconcat $ map (toHtml . go r) ns
        [whamlet|<article>#{content}|]
  where
    go r (NodeElement (Element "img" as [])) =
        NodeElement $ Element "img" (map (goA r) as) []
    go r (NodeElement (Element "a" as ns))
        | Just (slug', suffix) <- lookup "href" as >>= getSlugSuffix =
            NodeElement $ Element "a" (as' slug' suffix) $ map (go r) ns
      where
        as' slug' suffix =
            map helper as
          where
            helper ("href", _) = ("href", r (ChapterR slug') `T.append` suffix)
            helper p = p

        getSlugSuffix t = do
            guard $ ".dita#" `T.isInfixOf` t || ".dita" `T.isSuffixOf` t
            let (t', suffix') = T.breakOnEnd "#" t
                suffix = if T.null suffix' then T.empty else T.cons '#' suffix'
                x = T.breakOnEnd "/" $ fromMaybe t' $ T.stripSuffix "#" t'
            return (T.takeWhile (/= '.') $ snd x, suffix)
    go r (NodeElement (Element n as ns)) =
        NodeElement $ Element n as $ map (go r) ns
    go _ n = n

    goA r ("src", f) = ("src", r $ BookImageR $ either id id $ F.toText $ F.basename $ F.fromText f)
    goA _ p = p

getBookImageR :: Text -> Handler ()
getBookImageR name
    | name' == name'' =
        sendFile "image/png" $ F.encodeString $
            bookRoot F.</> "images" F.</> name' F.<.> "png"
    | otherwise = redirectWith status301 $ BookImageR $ either id id $ F.toText name'
  where
    name' = F.basename name''
    name'' = F.fromText name
