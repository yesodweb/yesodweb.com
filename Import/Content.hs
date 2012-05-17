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

import Prelude
    ( (.), ($), IO, Maybe (..), return, (==), (/=)
    , Eq, Show, Read
    , Either (..)
    )
import Data.Text (Text)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Filesystem as CB
import Text.Blaze.Html (Html, preEscapedToMarkup, preEscapedToMarkup, toHtml)
import qualified Data.ByteString as S
import Control.Applicative ((<$>))
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Filesystem.Path.CurrentOS ((</>), fromText, (<.>), FilePath)
import Filesystem (isFile)
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Yesod
    ( liftIO, GHandler, Yesod, RepHtml, notFound, defaultLayout
    , setTitle, toWidget
    , redirectWith
    )
import Network.HTTP.Types (status301)
import qualified Text.Markdown as Markdown
import Data.Maybe (fromMaybe)

data ContentFormat = ContentFormat
    { cfExtension :: Text
    , cfLoad :: C.Sink S.ByteString (C.ResourceT IO) (Either Text (Maybe Html, Html))
    -- ^ Left == redirect, Right == title, content
    }

-- | Turn a stream of 'S.ByteString's into an optional title line and the rest
-- of the text. Assumes UTF8 encoding.
sinkText :: (TL.Text -> a) -> C.Sink S.ByteString (C.ResourceT IO) (Either Text (Maybe Html, a))
sinkText f =
    Right . go . TL.fromChunks <$> (CT.decode CT.utf8 C.=$ CL.consume)
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
unsafeHtmlFormat = ContentFormat "html" $ sinkText $ preEscapedToMarkup

-- | Markdown content with XSS protection.
markdownFormat :: ContentFormat
markdownFormat = ContentFormat "md" $ sinkText $ Markdown.markdown Markdown.def

-- | 301 redirects
redirectFormat :: ContentFormat
redirectFormat = ContentFormat "redirect" $ Left . T.takeWhile (/= '\n') . T.concat <$> (CT.decode CT.utf8 C.=$ CL.consume)

-- | Try to load 'Html' from the given path.
loadContent :: FilePath -- ^ root
            -> [ContentFormat]
            -> ContentPath
            -> IO (Maybe (Either Text (Maybe Html, Html)))
loadContent _ [] _ = return Nothing
loadContent root (cf:cfs) cp@(ContentPath pieces) = do
    e <- isFile path
    if e
        -- FIXME caching
        then Just <$> (C.runResourceT $ CB.sourceFile path C.$$ cfLoad cf)
        else loadContent root cfs cp
  where
    path = foldl' go root pieces <.> cfExtension cf
    go x y = x </> fromText y

-- | Return some content as a 'Handler'.
returnContent :: Yesod master
              => FilePath -- ^ root
              -> Text -- ^ default title
              -> [ContentFormat]
              -> ContentPath
              -> GHandler sub master RepHtml
returnContent root defTitle cfs pieces = do
    mc <- liftIO $ loadContent root cfs pieces
    case mc of
        Nothing -> notFound
        Just (Left t) -> redirectWith status301 t
        Just (Right (mtitle, body)) -> defaultLayout $ do
            setTitle $ fromMaybe (toHtml defTitle) mtitle
            toWidget body

newtype ContentPath = ContentPath { unContentPath :: [Text] }
    deriving (Eq, Show, Read)
