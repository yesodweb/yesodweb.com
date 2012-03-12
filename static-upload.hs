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

data Path = Path Text L.ByteString S.ByteString
type Paths = [Path] -> [Path]

type M = RWST (Text, Manager) () (Set Text) IO

main :: IO ()
main = do
    let root = "http://localhost:3000"
    manager <- newManager def
    ((), _, ()) <- runRWST (download "/") (root, manager) Set.empty
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
    -- _ <- liftIO $ aws cfg manager po
    return ()

download :: Text -> M ()
download raw = do
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
                                    upload $ Path path (responseBody res) ct
                                    when ("text/html" `S.isPrefixOf` ct) $
                                        downloadHtmlRefs $ responseBody res
                                    when ("text/css" `S.isPrefixOf` ct) $
                                        downloadCssRefs $ responseBody res
                        else liftIO $ putStrLn $ "Received status code: " ++ show (statusCode res) ++ " for " ++ show path
        Nothing -> return ()

downloadHtmlRefs :: L.ByteString -> M ()
downloadHtmlRefs lbs = runResourceT
     $ CL.sourceList (L.toChunks lbs)
    $$ TS.tokenStream
    =$ CL.concatMap getRefs
    =$ CL.mapM_ download
  where
    getRefs :: TS.Token -> [Text]
    getRefs (TS.TagOpen _ as _) =
          map decodeUtf8
        $ (maybe id (:) $ lookup "href" as)
        $ (maybe id (:) $ lookup "src" as)
        []
    getRefs _ = []

downloadCssRefs :: L.ByteString -> M ()
downloadCssRefs lbs =
    case A.eitherResult $ A.parse parseUrls lbs of
        Left s -> liftIO $ putStrLn s
        Right urls -> do
            --liftIO $ putStrLn $ "CSS download: " ++ show (catMaybes urls)
            mapM_ (download . decodeUtf8) $ catMaybes urls
  where
    parseUrls = many parseUrl
    parseUrl = (do
        A.string "url("
        url <- A8.takeWhile1 (/= ')')
        A8.char ')'
        return $ Just url)
            <|> (A8.char 'u' >> return Nothing)
            <|> (A.skip (const True) >> A8.skipWhile (/= 'u') >> return Nothing)
