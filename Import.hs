module Import
    ( module Prelude
    , module Foundation
    , (<>)
    , Text
    , module Data.Monoid
    , module Control.Applicative
    , module Import.Content
    , module Settings.StaticFiles
    , getNewestBlog
    , getBlogList
    , prettyDay
    , loadBook
    , loadBlog
    ) where

import Prelude hiding (writeFile, readFile)
import Foundation
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text)
import Import.Content
import Settings.StaticFiles
import Data.Maybe (listToMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map as Map
import Data.Time
import System.Locale (defaultTimeLocale)
import qualified Book
import qualified Filesystem.Path.CurrentOS as F
import qualified Data.Yaml
import Settings (bookRoot, blogRoot)
import Data.IORef (readIORef)

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

getBlogList :: Handler [(Route YesodWeb, Post)]
getBlogList = do
    iblog <- ywBlog <$> getYesod
    Blog blog <- liftIO $ readIORef iblog
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

getNewestBlog :: Handler (Route YesodWeb, Post)
getNewestBlog = do
    iblog <- ywBlog <$> getYesod
    Blog blog <- liftIO $ readIORef iblog
    maybe notFound return $ listToMaybe $ do
        (year, x) <- take 1 $ reverse $ sortBy (comparing fst) $ Map.toList blog
        (month, y) <- take 1 $ reverse $ sortBy (comparing fst) $ Map.toList x
        (slug, post) <- take 1 y
        return (BlogPostR year month slug, post)

prettyDay :: UTCTime -> String
prettyDay = formatTime defaultTimeLocale "%B %e, %Y"

loadBook :: IO Book.Book
loadBook = Book.loadBook $ bookRoot F.</> "yesod-web-framework-book.toc"

loadBlog :: IO (Maybe Blog)
loadBlog = Data.Yaml.decodeFile $ F.encodeString $ blogRoot F.</> "posts.yaml"
