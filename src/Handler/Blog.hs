module Handler.Blog
    ( getBlogR
    , getBlogPostR
    , getOldBlogPostR
    , getFeedR
    ) where

import Import
import qualified Data.Map as Map
import qualified Data.ByteString as S
import Text.Blaze.Html (unsafeByteString)
import Settings (blogRoot, Author (..))
import Data.List (sortBy)
import Data.Ord (comparing)
import Yesod.Feed
import Text.Markdown (markdown, def, msXssProtect)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (fromStrict)
import qualified Data.Text as T
import Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Time (utctDay, toGregorian)
import qualified Data.Foldable as Fo
import RIO.FilePath

getBlogR :: Handler ()
getBlogR = getNewestBlog >>= redirect . fst

getOldBlogPostR :: Slug -> Handler ()
getOldBlogPostR s = do
    Blog blog <- getBlog
    case lookup s $ Fo.fold $ fmap Fo.fold blog of
        Nothing -> notFound
        Just post -> do
            let (y, m, _) = toGregorian $ utctDay $ postTime post
            redirect $ BlogPostR (fromIntegral y) (Month m) s

getBlogPostR :: Year -> Month -> Slug -> Handler Html
getBlogPostR y m s = do
    authors <- (ywAuthors <$> getYesod) >>= liftIO . readIORef
    Blog blog <- getBlog
    blog' <- maybe notFound return $ Map.lookup y blog
    blog'' <- maybe notFound return $ Map.lookup m blog'
    post <- maybe notFound return $ lookup s blog''
    let mauthor = Map.lookup (postAuthor post) authors
    content <- liftIO $ getContent post
    let currYear y' = y == y'
        currMonth y' m' = y == y' && m == m'
        currPost y' m' s' = y == y' && m == m' && s == s'
    if postRaw post
        then return content
        else defaultLayout $ do
            setTitle $ toHtml $ postTitle post
            let rev :: Ord k => Map.Map k v -> [(k, v)]
                rev = reverse . sortBy (comparing fst) . Map.toList
            $(widgetFile "blog") :: Widget
            $(widgetFile "archive")
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

getFeedR :: Handler TypedContent
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
        , feedAuthor = "Yesod Web Framework Team"
        , feedLogo = Nothing
        }
  where
    go (url, p) = do
        content <- liftIO $ getContent p
        return FeedEntry
            { feedEntryLink = url
            , feedEntryUpdated = postTime p
            , feedEntryTitle = postTitle p
            , feedEntryContent = content
            , feedEntryEnclosure = Nothing
            }

getContent :: Post -> IO Html
getContent post = do
    contentRaw <- S.readFile $ blogRoot </> postFP post
    return $
        if takeExtension (postFP post) == ".md"
            then markdown def { msXssProtect = False } $ fromStrict $ decodeUtf8 contentRaw
            else unsafeByteString contentRaw

gravatar :: Text -> Text
gravatar email = T.concat
    [ "https://www.gravatar.com/avatar/"
    , hashEmail email
    , "?size=80"
    ]
  where
    hashEmail = md5sum . T.toLower . T.strip
    md5sum = T.pack . show . md5 . L8.pack . T.unpack
