module Handler.Blog
    ( getBlogR
    , getBlogPostR
    , getFeedR
    ) where

import Import
import qualified Data.Map as Map
import qualified Data.ByteString as S
import qualified Filesystem.Path.CurrentOS as F
import Text.Blaze (unsafeByteString)
import Settings (blogRoot)
import Data.List (sortBy)
import Data.Ord (comparing)
import Yesod.Feed
import Data.IORef (readIORef)

getBlogR :: Handler ()
getBlogR = getNewestBlog >>= redirect . fst

getBlogPostR :: Year -> Month -> Slug -> Handler RepHtml
getBlogPostR y m s = do
    iblog <- ywBlog <$> getYesod
    Blog blog <- liftIO $ readIORef iblog
    blog' <- maybe notFound return $ Map.lookup y blog
    blog'' <- maybe notFound return $ Map.lookup m blog'
    post <- maybe notFound return $ lookup s blog''
    content <- liftIO $ S.readFile $ F.encodeString $ blogRoot F.</> postFP post
    let currYear y' = y == y'
        currMonth y' m' = y == y' && m == m'
        currPost y' m' s' = y == y' && m == m' && s == s'
    defaultLayout $ do
        setTitle $ toHtml $ postTitle post
        let rev :: Ord k => Map.Map k v -> [(k, v)]
            rev = reverse . sortBy (comparing fst) . Map.toList
        $(widgetFile "blog")
  where
    pretty 1 = "January"
    pretty 2 = "February"
    pretty 3 = "March"
    pretty 4 = "April"
    pretty 5 = "May"
    pretty 6 = "June"
    pretty 7 = "July"
    pretty 8 = "August"
    pretty 9 = "September"
    pretty 10 = "October"
    pretty 11 = "November"
    pretty 12 = "December" :: Text
    pretty _ = "Some new month"

getFeedR :: Handler RepAtomRss
getFeedR = do
    posts <- getBlogList
    f <-
        case posts of
            [] -> notFound
            (_, f):_ -> return f
    entries <- mapM go $ take 5 posts
    newsFeed Feed
        { feedTitle = "Yesod Web Framework Blog"
        , feedLinkSelf = FeedR
        , feedLinkHome = RootR
        , feedDescription = "Development blog for the Yesod Web Framework"
        , feedLanguage = "en"
        , feedUpdated = postTime f
        , feedEntries = entries
        }
  where
    go (url, p) = do
        content <- liftIO $ S.readFile $ F.encodeString $ blogRoot F.</> postFP p
        return FeedEntry
            { feedEntryLink = url
            , feedEntryUpdated = postTime p
            , feedEntryTitle = postTitle p
            , feedEntryContent = unsafeByteString content
            }
