module Import
    ( module X
    , getNewestBlog
    , getBlogList
    , prettyDay
    , loadBook
    , loadBlog
    , loadAuthors
    , getBlog
    ) where

import RIO as X hiding (Handler (..), logWarn, logWarnS, logError, logErrorS, logInfo, logInfoS, logDebug, logDebugS, logOther, logOtherS, LogLevel (..))
import Foundation as X
import Import.Content as X
import Settings.StaticFiles as X
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Time
import qualified Book
import qualified Data.Yaml
import Settings (blogRoot, Author)
import RIO.FilePath ((</>))

getBlogList :: Handler [(Route YesodWeb, Post)]
getBlogList = do
    Blog blog <- getBlog
    return $ concatMap go' $ concatMap go $ reverse $ Map.toList blog
  where
    go :: (a, Map.Map b c) -> [(a, b, c)]
    go (a, m) = do
        (b, c) <- reverse $ Map.toList m
        return (a, b, c)
    go' :: (Year, Month, [(Slug, Post)]) -> [(Route YesodWeb, Post)]
    go' (y, m, ps) = do
        (s, p) <- ps
        return (BlogPostR y m s, p)

getBlog :: Handler Blog
getBlog = do
    now <- liftIO getCurrentTime
    (ywBlog <$> getYesod) >>= fmap (filterBlog now) . liftIO . readIORef

getNewestBlog :: Handler (Route YesodWeb, Post)
getNewestBlog = do
    Blog blog <- getBlog
    maybe notFound return $ listToMaybe $ do
        (year, x) <- take 1 $ reverse $ sortBy (comparing fst) $ Map.toList blog
        (month, y) <- take 1 $ reverse $ sortBy (comparing fst) $ Map.toList x
        (slug, post) <- take 1 y
        return (BlogPostR year month slug, post)

prettyDay :: UTCTime -> String
prettyDay time
    | month == 4 && day == 1 = "March 32, " ++ show year
    | otherwise = formatTime defaultTimeLocale "%B %e, %Y" time
  where
    (year, month, day) = toGregorian $ utctDay time

loadBook :: FilePath -> IO (Either SomeException Book.Book)
loadBook root = Book.loadBook root

loadBlog :: IO (Either Data.Yaml.ParseException Blog)
loadBlog = Data.Yaml.decodeFileEither $ blogRoot </> "posts.yaml"

loadAuthors :: IO (Map.Map Text Author)
loadAuthors = either (const Map.empty) id <$> Data.Yaml.decodeFileEither (blogRoot </> "authors.yaml")

