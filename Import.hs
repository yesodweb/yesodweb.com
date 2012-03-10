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
    , prettyDay
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

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

getNewestBlog :: Handler (Route YesodWeb, Post)
getNewestBlog = do
    Blog blog <- ywBlog <$> getYesod
    maybe notFound return $ listToMaybe $ do
        (year, x) <- take 1 $ reverse $ sortBy (comparing fst) $ Map.toList blog
        (month, y) <- take 1 $ reverse $ sortBy (comparing fst) $ Map.toList x
        (slug, post) <- take 1 y
        return (BlogPostR year month slug, post)

prettyDay :: UTCTime -> String
prettyDay = formatTime defaultTimeLocale "%B %e, %Y"
