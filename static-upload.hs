{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Conduit
import Data.Conduit
import qualified Data.Conduit.List as CL
import System.Environment (getArgs)
import Control.Monad.Trans.RWS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, catMaybes)
import qualified Text.HTML.TagStream as TS
import Control.Monad.IO.Class (liftIO)
import Aws
import Aws.S3
import Control.Monad (when)
import Network.HTTP.Types (status200)
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Control.Applicative ((<|>), many)
import qualified Text.XML.Stream.Parse as P
import qualified Text.XML.Stream.Render as R
import Data.XML.Types
import System.IO.Unsafe (unsafePerformIO)

data Path = Path Text L.ByteString S.ByteString
type Paths = [Path] -> [Path]

type M = RWST (Text, Manager) () (Set Text) IO

main :: IO ()
main = do
    let root = "http://localhost:3000"
    manager <- newManager def
    ((), _, ()) <- runRWST (download Nothing "/") (root, manager) Set.empty
    return ()

upload :: Path -> M ()
upload (Path obj contents mime) = do
    --liftIO $ putStrLn $ "Uploading " ++ show obj
    (_, manager) <- ask
    let po' = putObject obj "www.yesodweb.com" (RequestBodyLBS contents)
        po = po'
            { poContentType = Just mime
            , poAcl = Just AclPublicRead
            }
    cfg <- liftIO baseConfiguration
    _ <- liftIO $ aws cfg manager po
    return ()

download :: Maybe Text -> Text -> M ()
download msource raw' = do
    let raw =
            case (msource >>= T.stripPrefix "wiki/", "/" `T.isInfixOf` raw', ":" `T.isInfixOf` raw') of
                (Just{}, False, False) -> "/wiki/" `T.append` raw'
                _ -> raw'
    (root, manager) <- ask
    case T.stripPrefix "/" $ fromMaybe raw $ T.stripPrefix root raw of
        Just noRoot -> do
            let path' =
                    case noRoot of
                        "" -> "index.html"
                        x -> x
                path = T.takeWhile (\c -> c /= '?' && c /= '#') path'
            visited <- get
            if path `Set.member` visited
                then return ()
                else do
                    put $ Set.insert path visited
                    url <- parseUrl $ T.unpack $ T.append root
                                    $ T.takeWhile (/= '#')
                                    $ T.replace "%20" " " raw
                    --liftIO $ putStrLn $ "Downloading: " ++ show raw ++ " to " ++ show path
                    res <- runResourceT $ httpLbs url
                        { checkStatus = \_ _ -> Nothing
                        } manager
                    if (statusCode res == status200)
                        then
                            case lookup "content-type" $ responseHeaders res of
                                Just ct -> do
                                    let toUpload =
                                            if ct == "application/atom+xml"
                                                then fixAtom $ responseBody res
                                                else responseBody res
                                    upload $ Path path toUpload ct
                                    when ("text/html" `S.isPrefixOf` ct) $
                                        downloadHtmlRefs path $ responseBody res
                                    when ("text/css" `S.isPrefixOf` ct) $
                                        downloadCssRefs path $ responseBody res
                        else liftIO $ putStrLn $ "Received status code: " ++ show (statusCode res) ++ " for " ++ show path ++ ", referenced from: " ++ show msource
        Nothing -> return ()

downloadHtmlRefs :: Text -> L.ByteString -> M ()
downloadHtmlRefs path lbs = runResourceT
     $ CL.sourceList (L.toChunks lbs)
    $$ TS.tokenStream
    =$ CL.concatMap getRefs
    =$ CL.mapM_ (download (Just path))
  where
    getRefs :: TS.Token -> [Text]
    getRefs (TS.TagOpen _ as _) =
          map decodeUtf8
        $ (maybe id (:) $ lookup "href" as)
        $ (maybe id (:) $ lookup "src" as)
        []
    getRefs _ = []

downloadCssRefs :: Text -> L.ByteString -> M ()
downloadCssRefs path lbs =
    case A.eitherResult $ A.parse parseUrls lbs of
        Left s -> liftIO $ putStrLn s
        Right urls -> do
            --liftIO $ putStrLn $ "CSS download: " ++ show (catMaybes urls)
            mapM_ (download (Just path) . decodeUtf8) $ catMaybes urls
  where
    parseUrls = many parseUrl
    parseUrl = (do
        A.string "url("
        url <- A8.takeWhile1 (/= ')')
        A8.char ')'
        return $ Just url)
            <|> (A8.char 'u' >> return Nothing)
            <|> (A.skip (const True) >> A8.skipWhile (/= 'u') >> return Nothing)

fixAtom :: L.ByteString -> L.ByteString
fixAtom lbs =
       L.fromChunks
     $ unsafePerformIO
     $ runResourceT
     $ P.parseLBS P.def lbs
    $$ CL.map fixEvent
    =$ R.renderBytes R.def
    =$ CL.consume
  where
    fixEvent (EventBeginElement n as) = EventBeginElement n $ map fixA as
    fixEvent e = e

    fixA ("href", t) = ("href", ContentText "http://www.yesodweb.com" : t)
    fixA a = a
