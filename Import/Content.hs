{-# LANGUAGE RankNTypes #-}
-- | Load page contents from the content folder.
module Import.Content
    ( ContentFormat (..)
    , ContentPath (..)
    , htmlFormat
    , unsafeHtmlFormat
    , markdownFormat
    , loadContent
    , returnContent
    ) where

import Prelude
    ( (.), ($), IO, Maybe (..), return, (==), maybe
    , Eq, Show, Read
    )
import Data.Text (Text, splitOn, intercalate)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Filesystem as CB
import Text.Blaze (Html, preEscapedText, preEscapedLazyText, toHtml)
import qualified Data.ByteString as S
import Control.Applicative ((<$>))
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Filesystem.Path.CurrentOS ((</>), fromText, (<.>), FilePath)
import Filesystem (isFile)
import Data.List (foldl')
import qualified Data.Text.Lazy as TL
import Control.Arrow (second)
import Yesod
    ( liftIO, GHandler, Yesod, RepHtml, notFound, defaultLayout
    , setTitle, toWidget
    , PersistField (..)
    )
import qualified Text.Markdown as Markdown
import Database.Persist.Store (SqlType (SqlString))

data ContentFormat = ContentFormat
    { cfExtension :: Text
    , cfLoad :: C.Sink S.ByteString IO (Maybe Html, Html)
    }

-- | Turn a stream of 'S.ByteString's into an optional title line and the rest
-- of the text. Assumes UTF8 encoding.
sinkText :: C.Sink S.ByteString IO (Maybe Html, TL.Text)
sinkText =
    go . TL.fromChunks <$> (CT.decode CT.utf8 C.=$ CL.consume)
  where
    go t =
        case TL.stripPrefix "title: " x of
            Just title -> (Just $ toHtml title, TL.drop 1 y)
            Nothing -> (Nothing, t)
      where
        (x, y) = TL.break (== '\n') t

-- | HTML content with XSS protection.
htmlFormat :: ContentFormat
htmlFormat = ContentFormat "html" $
    second (preEscapedText . sanitizeBalance . TL.toStrict) <$> sinkText

-- | HTML content without XSS protection.
unsafeHtmlFormat :: ContentFormat
unsafeHtmlFormat = ContentFormat "html" $
    second preEscapedLazyText <$> sinkText

-- | Markdown content with XSS protection.
markdownFormat :: ContentFormat
markdownFormat = ContentFormat "md" $
    second (Markdown.markdown Markdown.def) <$> sinkText

-- | Try to load 'Html' from the given path.
loadContent :: FilePath -- ^ root
            -> [ContentFormat]
            -> ContentPath
            -> IO (Maybe (Maybe Html, Html))
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
              -> [ContentFormat]
              -> ContentPath
              -> GHandler sub master RepHtml
returnContent root cfs pieces = do
    mc <- liftIO $ loadContent root cfs pieces
    case mc of
        Nothing -> notFound
        Just (mtitle, body) -> defaultLayout $ do
            maybe (return ()) setTitle mtitle
            toWidget body

newtype ContentPath = ContentPath { unContentPath :: [Text] }
    deriving (Eq, Show, Read)
instance PersistField ContentPath where
    toPersistValue = toPersistValue . intercalate "/" . unContentPath
    fromPersistValue v = do
        t <- fromPersistValue v
        return $ ContentPath $ splitOn "/" t
    sqlType _ = SqlString
