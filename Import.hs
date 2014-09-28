module Import
    ( module X
    , (<>)
    , Text
    , getNewestBlog
    , getBlogList
    , prettyDay
    , loadBook
    , loadBlog
    , loadAuthors
    , getBlog
    ) where

import Prelude as X hiding (writeFile, readFile)
import Foundation as X
import Data.Monoid as X (Monoid (mappend, mempty, mconcat))
import Control.Applicative as X ((<$>), (<*>), pure)
import Data.Text (Text)
import Import.Content as X
import Settings.StaticFiles as X
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map as Map
import Data.Time
import System.Locale (defaultTimeLocale)
import qualified Book
import qualified Filesystem.Path.CurrentOS as F
import qualified Data.Yaml
import Settings (blogRoot, Author)
import Data.IORef (readIORef)

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

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
prettyDay = formatTime defaultTimeLocale "%B %e, %Y"

loadBook :: F.FilePath -> IO Book.Book
loadBook root = Book.loadBook $ root F.</> "yesod-web-framework-book.asciidoc"

loadBlog :: IO (Either Data.Yaml.ParseException Blog)
loadBlog = Data.Yaml.decodeFileEither $ F.encodeString $ blogRoot F.</> "posts.yaml"

loadAuthors :: IO (Map.Map Text Author)
loadAuthors = fromMaybe Map.empty <$> Data.Yaml.decodeFile (F.encodeString $ blogRoot F.</> "authors.yaml")

