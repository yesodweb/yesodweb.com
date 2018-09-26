{-# LANGUAGE RankNTypes #-}
-- | Load page contents from the content folder.
module Import.Content
    ( ContentFormat (..)
    , ContentPath (..)
    , htmlFormat
    , unsafeHtmlFormat
    , markdownFormat
    , redirectFormat
    , loadContent
    , returnContent
    ) where

import Conduit
import Text.Blaze.Html (Html, preEscapedToMarkup, preEscapedToMarkup, toHtml)
import Text.HTML.SanitizeXSS (sanitizeBalance)
import RIO
import RIO.FilePath
import RIO.Directory
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Yesod
    ( liftIO, HandlerFor, Yesod, notFound, defaultLayout
    , setTitle
    , redirectWith
    )
import Network.HTTP.Types (status301)
import qualified Text.Markdown as Markdown
import Data.Maybe (fromMaybe)
import Settings (widgetFile)

data ContentFormat = ContentFormat
    { cfExtension :: Text
    , cfLoad :: ConduitT ByteString Void IO (Either Text (Maybe Html, Html))
    -- ^ Left == redirect, Right == title, content
    }

-- | Turn a stream of 'ByteString's into an optional title line and the rest
-- of the text. Assumes UTF8 encoding.
sinkText :: (TL.Text -> a) -> ConduitT ByteString Void IO (Either Text (Maybe Html, a))
sinkText f =
    Right . go . TL.fromChunks <$> (decodeUtf8C .| sinkList)
  where
    go t =
        case TL.stripPrefix "Title: " x of
            Just title -> (Just $ toHtml title, f $ TL.drop 1 y)
            Nothing -> (Nothing, f t)
      where
        (x, y) = TL.break (== '\n') t

-- | HTML content with XSS protection.
htmlFormat :: ContentFormat
htmlFormat = ContentFormat "html" $ sinkText $
    preEscapedToMarkup . sanitizeBalance . TL.toStrict

-- | HTML content without XSS protection.
unsafeHtmlFormat :: ContentFormat
unsafeHtmlFormat = ContentFormat "html" $ sinkText preEscapedToMarkup

-- | Markdown content with XSS protection.
markdownFormat :: ContentFormat
markdownFormat = ContentFormat "md" $ sinkText $ Markdown.markdown Markdown.def

-- | 301 redirects
redirectFormat :: ContentFormat
redirectFormat = ContentFormat "redirect" $ Left . T.takeWhile (/= '\n') . T.concat <$> (decodeUtf8C .| sinkList)

-- | Try to load 'Html' from the given path.
loadContent :: FilePath -- ^ root
            -> [ContentFormat]
            -> ContentPath
            -> IO (Maybe (Either Text (Maybe Html, Html)))
loadContent _ [] _ = return Nothing
loadContent root (cf:cfs) cp@(ContentPath pieces) = do
    e <- doesFileExist path
    if e
        -- FIXME caching
        then Just <$> withSourceFile path (\src -> runConduit $ src .| cfLoad cf)
        else loadContent root cfs cp
  where
    path = foldl' go root pieces <.> T.unpack (cfExtension cf)
    go x y = x </> T.unpack y

-- | Return some content as a 'Handler'.
returnContent :: Yesod master
              => FilePath -- ^ root
              -> Text -- ^ default title
              -> [ContentFormat]
              -> ContentPath
              -> HandlerFor master Html
returnContent root defTitle cfs pieces = do
    mc <- liftIO $ loadContent root cfs pieces
    case mc of
        Nothing -> notFound
        Just (Left t) -> redirectWith status301 t
        Just (Right (mtitle, body)) -> defaultLayout $ do
            setTitle $ fromMaybe (toHtml defTitle) mtitle
            $(widgetFile "page")

newtype ContentPath = ContentPath { unContentPath :: [Text] }
    deriving (Eq, Show, Read)
