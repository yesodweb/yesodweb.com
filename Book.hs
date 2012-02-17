{-# LANGUAGE ScopedTypeVariables #-}
module Book
    ( Book (..)
    , Part (..)
    , Chapter (..)
    , loadBook
    ) where

import Prelude
import qualified Data.Map as Map
import Data.Text (Text)
import Text.XML
import qualified Filesystem.Path.CurrentOS as F
import Control.Exception (handle, SomeException, throw)

data Book = Book
    { bookParts :: [Part]
    , bookChapterMap :: Map.Map Text Chapter
    }

data Part = Part
    { partTitle :: Text
    , partChapters :: [Chapter]
    }

data Chapter = Chapter
    { chapterTitle :: Text
    , chapterPath :: F.FilePath
    , chapterSlug :: Text
    }

loadBook :: F.FilePath -> IO Book
loadBook fp = handle (\(e :: SomeException) -> return (throw e)) $ do
    Document _ (Element _ _ parts') _ <- Text.XML.readFile def fp
    parts <- mapM parsePart parts'
    let m = Map.fromList $ concatMap goP parts
        goC c = (chapterSlug c, c)
        goP = map goC . partChapters
    return $ Book parts m
  where
    dir = F.directory fp

    parsePart :: Node -> IO Part
    parsePart (NodeElement (Element "nav" [("title", title)] chapters')) = do
        chapters <- mapM parseChapter chapters'
        return $ Part title chapters

    parseChapter :: Node -> IO Chapter
    parseChapter (NodeElement (Element "nav" as []))
      | Just title <- lookup "title" as, Just href' <- lookup "href" as = do
        let href = dir F.</> F.fromText href'
            slug = either id id $ F.toText $ F.basename href
        return $ Chapter title href slug
