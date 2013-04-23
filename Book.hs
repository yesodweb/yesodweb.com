{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Book
    ( Book (..)
    , Part (..)
    , Chapter (..)
    , loadBook
    ) where

import Prelude
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Text.XML as X
import qualified Filesystem.Path.CurrentOS as F
import Control.Exception (handle, SomeException, throw)
import Text.Blaze.Html (Html, toHtml, unsafeLazyByteString)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.ByteString.Lazy as L
import Data.Monoid (mconcat)
import System.IO (hPutStrLn, stderr)
import qualified Data.Text as T
import Data.Char (isUpper)
import Control.Exception (evaluate)

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
    , chapterHtml :: Html
    }

getTitle =
    go id
  where
    go _ [] = error "Title not found"
    go front ((NodeElement (Element "title" _ [NodeContent t])):rest) =
        return (t, front rest)
    go front (n:ns) = go (front . (n:)) ns

loadBook :: F.FilePath -> IO Book
loadBook fp = handle (\(e :: SomeException) -> return (throw e)) $ do
    Document _ (Element _ _ parts') _ <- X.readFile def fp
    parts <- fmap concat $ mapM parsePart parts'
    let m = Map.fromList $ concatMap goP parts
        goC c = (chapterSlug c, c)
        goP = map goC . partChapters
    return $ Book parts m
  where
    dir = F.directory fp

    parsePart :: Node -> IO [Part]
    parsePart (NodeElement (Element "part" as cs)) = do
        (title, chapters') <- getTitle cs
        chapters <- mapM parseChapter chapters'
        return [Part title $ concat chapters]
    parsePart (NodeElement (Element "title" _ _)) = return []
    parsePart NodeContent{} = return []
    parsePart _ = error "Book.parsePart"

    parseChapter :: Node -> IO [Chapter]
    parseChapter (NodeElement (Element "{http://www.w3.org/2001/XInclude}include" as _)) = do
        Just href <- return $ Map.lookup "href" as
        let fp = dir F.</> F.fromText href
            slug = either id id $ F.toText $ F.basename fp
        if slug == "ch00"
            then return []
            else do
                Document _ (Element _ _ cs) _ <- X.readFile def fp
                (title, rest) <- getTitle cs
                let content = mconcat $ map toHtml $ concatMap (goNode False) rest
                !_ <- evaluate $ L.length $ renderHtml content -- FIXME comment out to avoid eager error checking
                return [Chapter title fp slug content]
    parseChapter NodeContent{} = return []
    parseChapter _ = error "Book.parseChapter"

    goNode :: Bool -> Node -> [Node]
    goNode b (NodeElement e) = goElem b e
    goNode _ n = [n]

    goElem :: Bool -- ^ inside figure?
           -> Element -> [Node]
    {-
    goElem _ (Element "apiname" _ [NodeContent t]) = goApiname t
    -}
    goElem _ (Element "programlisting" as [NodeContent t]) | Just "lhaskell" <- Map.lookup "language" as = goLHS t
    goElem _ (Element "programlisting" as [NodeContent t]) | Just "haskell" <- Map.lookup "language" as =
        [NodeElement $ Element "pre" as [NodeElement $ Element "code" Map.empty [NodeContent $ goStartStop t]]]
    goElem insideFigure (Element n as cs)
    {-
        | insideFigure && n == "h1" = [NodeElement $ Element "figcaption" as cs']
        -}
        | nameLocalName n `Set.member` unchanged = [NodeElement $ Element n as cs']
        | Just n' <- Map.lookup n simples = [NodeElement $ Element n' as cs']
        | Just (n', clazz) <- Map.lookup n classes = [NodeElement $ Element n' (Map.insert "class" clazz as) cs']
        | n `Set.member` deleted = []
        | n `Set.member` stripped = cs'
        | n == "programlisting" = [NodeElement $ Element "pre" as [NodeElement $ Element "code" Map.empty cs']]
        | n == "imagedata" = goImage as cs'
        | n == "varlistentry" = goVarListEntry insideFigure cs
        | n == "ulink", Just url <- Map.lookup "url" as =
            [ NodeElement $ Element
                "a"
                (Map.delete "url" $ Map.insert "href" url as)
                cs'
            ]
        | otherwise = error $ "Unknown: " ++ show (nameLocalName n)
      where
        cs' = concatMap (goNode $ insideFigure || n == "figure") cs

    unchanged = Set.fromList $ T.words "section figure table tgroup thead tbody blockquote code"

    simples = Map.fromList
        [ ("para", "p")
        , ("emphasis", "em")
        , ("title", "h1")
        , ("itemizedlist", "ul")
        , ("orderedlist", "ol")
        , ("listitem", "li")
        , ("link", "a")
        , ("variablelist", "dl")
        , ("term", "dt")
        , ("literal", "code")
        , ("quote", "q")
        , ("row", "tr")
        , ("entry", "td")
        , ("citation", "cite")
        ]
        {-
        , ("title", "h1")
        ]
        -}

    classes = Map.fromList
        [ ("glossterm", ("span", "term"))
        , ("function", ("span", "apiname"))
        , ("command", ("span", "cmdname"))
        , ("note", ("aside", "note"))
        , ("userinput", ("span", "userinput"))
        , ("varname", ("span", "varname"))
        , ("filename", ("span", "filepath"))
        ]
        {-
        , ("msgblock", ("pre", "msgblock"))
        ]
        -}

    deleted = Set.fromList
        [
        ]

    stripped = Set.fromList
        [ "mediaobject"
        , "imageobject"
        ]
        {-
        [ "conbody"
        , "dlentry"
        ]
        -}

    goVarListEntry insideFigure =
        concatMap go
      where
        go (NodeElement (Element "term" _ cs)) =
            [ NodeElement $ Element "dt" Map.empty $
                concatMap (goNode insideFigure) cs
            ]
        go (NodeElement (Element "listitem" _ cs)) =
            [ NodeElement $ Element "dd" Map.empty $
                concatMap (goNode insideFigure) cs
            ]
        go _ = []

    goApiname t =
        [NodeElement $ Element "a" (Map.singleton "href" href) [NodeContent text]]
      where
        (href, text) =
            case T.split (== ':') t of
                [x] -> (T.append "http://hackage.haskell.org/package/" x, x)
                [x, y] -> (T.concat
                    [ "http://hackage.haskell.org/packages/archive/"
                    , x
                    , "/latest/doc/html/"
                    , T.replace "." "-" y
                    , ".html"
                    ], y)
                [x, y, z] -> (T.concat
                    [ "http://hackage.haskell.org/packages/archive/"
                    , x
                    , "/latest/doc/html/"
                    , T.replace "." "-" y
                    , ".html#"
                    , if isUpper (T.head z) then "t:" else "v:"
                    , z
                    ], z)
                xs -> error $ show xs

    goXref as cs =
        [NodeElement $ Element "a" (Map.singleton "href" href') cs]
      where
        href' =
            case Map.lookup "href" as of
                Just href
                    | "/" `T.isPrefixOf` href || "://" `T.isInfixOf` href -> href
                    | otherwise ->
                        let name = T.takeWhile (/= '.') href
                            suffix = T.dropWhile (/= '#') href
                         in T.append name suffix
                Nothing -> error "xref with href"

    goStartStop t
        | "-- START" `elem` ls = T.unlines $ go False ls
        | otherwise = t
      where
        ls = T.lines t

        go _ [] = []
        go _ ("-- START":xs) = go True xs
        go _ ("-- STOP":xs) = go False xs
        go True (x:xs) = x : go True xs
        go False (_:xs) = go False xs

    goLHS t0 =
        map go lhBlocks
      where
        ls = T.lines t0
        lhLines = map lhLine ls
        lhBlocks = map (fmap T.unlines) $ toBlocks lhLines

        go (LHCode t) = NodeElement $ Element "pre" Map.empty [NodeElement $ Element "code" Map.empty [NodeContent t]]
        go (LHText t) = NodeElement $ Element "p" (Map.singleton "style" "white-space:pre") [NodeContent t]

    goImage as cs =
        [NodeElement $ Element "img" (Map.singleton "src" href') cs]
      where
        href' =
            case Map.lookup "fileref" as of
                Just href ->
                    let name = either id id $ F.toText $ F.basename $ F.fromText href
                     in T.append "image/" name
                Nothing -> error "image without href"

data LHask t = LHCode t | LHText t

instance Functor LHask where
    fmap f (LHCode x) = LHCode (f x)
    fmap f (LHText x) = LHText (f x)

lhLine :: Text -> LHask [Text]
lhLine t =
    case T.stripPrefix "> " t of
        Nothing -> LHText [t]
        Just s -> LHCode [s]

toBlocks :: [LHask [Text]] -> [LHask [Text]]
toBlocks [] = []
toBlocks (LHCode x:LHCode y:rest) = toBlocks $ LHCode (x ++ y) : rest
toBlocks (LHText x:LHText y:rest) = toBlocks $ LHText (x ++ y) : rest
toBlocks (x:rest) = x : toBlocks rest
