{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Book
    ( Book (..)
    , Part (..)
    , Chapter (..)
    , loadBook
    ) where

import           Control.Monad.Trans.Writer
import qualified Data.ByteString.Lazy          as L
import           Conduit
import qualified Data.Map                      as Map
import           Data.Maybe                    (listToMaybe, mapMaybe)
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import           RIO
import           RIO.Directory
import           RIO.FilePath
import           Text.Blaze.Html               (Html, toHtml)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.XML                      as X

data Book = Book
    { bookParts      :: [Part]
    , bookChapterMap :: Map Text Chapter
    }

data Part = Part
    { partTitle    :: Text
    , partChapters :: [Chapter]
    }

data Chapter = Chapter
    { chapterTitle :: Text
    , chapterPath  :: FilePath
    , chapterSlug  :: Text
    , chapterHtml  :: Html
    }

getTitle :: [Node] -> IO (Text, [Node])
getTitle =
    go id
  where
    go _ [] = error "Title not found"
    go front (NodeElement (Element "title" _ [NodeContent t]):rest) =
        return (t, front rest)
    go front (n:ns) = go (front . (n:)) ns

mapWhile :: Monad m => (a -> Maybe b) -> ConduitT a b m ()
mapWhile f =
    loop
  where
    loop = await >>= maybe (return ()) go
    go x =
        case f x of
            Just y -> yield y >> loop
            Nothing -> leftover x

data BookLine = Title Text | Include Text

parseBookLine :: Text -> Maybe BookLine
parseBookLine t
    | Just t' <- T.stripPrefix "= " t = Just $ Title t'
    | Just t' <- T.stripPrefix "include::chapters/" t
             >>= T.stripSuffix ".asciidoc[]" = Just $ Include t'
    | Just t' <- T.stripPrefix "include::" t
             >>= T.stripSuffix ".asciidoc[]" = Just $ Include t'
    | otherwise = Nothing

loadBook :: FilePath -> IO (Either SomeException Book)
loadBook dir = try $ do
    fp <- trySuffixes
        [ "yesod-web-framework-book.asciidoc"
        , "asciidoc/book.asciidoc"
        ]
    parts <- withSourceFile fp
           $ \src -> runConduit
           $ src
          .| decodeUtf8C
          .| linesUnboundedC
          .| concatMapC parseBookLine
          .| (dropC 1 >> parseParts)
          .| sinkList
    let m = Map.fromList $ concatMap goP parts
        goC c = (chapterSlug c, c)
        goP = map goC . partChapters
    return $ Book parts m
  where
    trySuffixes [] = error "No suffixes worked for loading book"
    trySuffixes (x:xs) = do
        let fp = dir </> x
        exists <- doesFileExist fp
        if exists then return fp else trySuffixes xs

    parseParts :: ConduitT BookLine Part IO ()
    parseParts =
        start
      where
        start = await >>= maybe (return ()) start'

        start' (Title t) = do
            let getInclude (Include x) = Just x
                getInclude _ = Nothing
            chapters <- mapWhile getInclude .| mapMC parseChapter .| sinkList
            yield $ Part t chapters
            start
        start' (Include _t) = start -- error $ "Invalid beginning of a Part: " ++ show t

    ps = def { psDecodeEntities = decodeHtmlEntities }

    -- Read a chapter as an XML file, converting from AsciiDoc as necessary
    chapterToDoc fp'
        | takeExtension fp' `elem` [".ad", ".asciidoc"] =
            let fp'' = takeDirectory fp' </> "../generated-xml" </> takeBaseName fp' <.> "xml"
             in X.readFile ps fp''
        | otherwise = X.readFile ps fp'

    getSection (NodeElement e@(Element "section" _ _)) = Just e
    getSection (NodeElement e@(Element "appendix" _ _)) = Just e
    getSection _ = Nothing

    parseChapter :: Text -> IO Chapter
    parseChapter slug = do
        let fp' = dir </> "generated-xml" </> T.unpack slug <.> "xml"
        Document _ (Element name _ csOrig) _ <- chapterToDoc fp'
        cs <-
            case name of
                "article" ->
                    case listToMaybe $ mapMaybe getSection csOrig of
                        Nothing -> error "article without child section"
                        Just (Element _ _ cs) -> return cs
                "chapter" -> return csOrig
                _ -> error $ "Unknown root element: " ++ show name
        (title, rest) <- getTitle cs
        let content = mconcat $ map toHtml $ concatMap (goNode False) rest
        !_ <- evaluate $ L.length $ renderHtml content -- FIXME comment out to avoid eager error checking
        return $ Chapter title fp' slug content

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
        [ NodeElement $ Element "pre" as
            [ NodeElement $ Element
                "code"
                (Map.singleton "class" $ T.unwords $ execWriter $ do
                    tell ["haskell"]
                    case filter ("main = " `T.isPrefixOf`) $ T.lines t of
                        rest:_ -> do
                            tell ["active"]
                            when ("warp" `elem` T.words rest) $ tell ["web"]
                        [] -> return ()
                    )
                [ NodeContent $ goStartStop t
                ]
            ]
        ]
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
        , ("simpara", "p")
        , ("emphasis", "em")
        , ("title", "h1")
        , ("itemizedlist", "ul")
        , ("orderedlist", "ol")
        , ("listitem", "li")
        , ("link", "a")
        , ("variablelist", "dl")
        , ("term", "dt")
        , ("literal", "code")
        , ("screen", "pre")
        , ("quote", "q")
        , ("row", "tr")
        , ("entry", "td")
        , ("citation", "cite")
        , ("literallayout", "pre")
        , ("informaltable", "table")
        , ("formalpara", "section")
        , ("attribution", "cite")
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
        [ "textobject"
        , "colspec"
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
                    let name = T.pack $ takeBaseName $ T.unpack href
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
