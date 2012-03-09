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
import Data.Maybe (fromMaybe)
import qualified Text.HTML.TagStream as TS
import Control.Monad.IO.Class (liftIO)
import Aws
import Aws.S3
import Control.Monad (when)
import Network.HTTP.Types (status200)

data Path = Path Text L.ByteString S.ByteString
type Paths = [Path] -> [Path]

type M = RWST (Text, Manager) () (Set Text) IO

main :: IO ()
main = do
    args <- getArgs
    root <-
        case args of
            [x] -> return $ T.pack x
            _ -> error "Invalid args"
    manager <- newManager def
    ((), _, ()) <- runRWST (download "/") (root, manager) Set.empty
    return ()

upload :: Path -> M ()
upload (Path obj contents mime) = do
    liftIO $ putStrLn $ "Uploading " ++ show obj
    (_, manager) <- ask
    let po' = putObject obj "www.yesodweb.com" (RequestBodyLBS contents)
        po = po'
            { poContentType = Just mime
            , poAcl = Just AclPublicRead
            }
    cfg <- liftIO baseConfiguration
    _ <- liftIO $ aws cfg manager po
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
                    url <- parseUrl $ T.unpack $ T.append root $ T.takeWhile (/= '#') raw
                    liftIO $ putStrLn $ "Downloading: " ++ show raw ++ " to " ++ show path
                    res <- runResourceT $ httpLbs url
                        { checkStatus = \_ _ -> Nothing
                        } manager
                    if (statusCode res == status200)
                        then
                            case lookup "content-type" $ responseHeaders res of
                                Just ct -> do
                                    upload $ Path path (responseBody res) ct
                                    when ("text/html" `S.isPrefixOf` ct) $
                                        downloadRefs $ responseBody res
                        else liftIO $ putStrLn $ "Received status code: " ++ show (statusCode res)
        Nothing -> return ()

downloadRefs :: L.ByteString -> M ()
downloadRefs lbs = runResourceT
     $ CL.sourceList (L.toChunks lbs)
    $$ TS.tokenStream
    =$ CL.concatMap getRefs
    =$ CL.mapM_ download

getRefs :: TS.Token -> [Text]
getRefs (TS.TagOpen _ as _) =
      map decodeUtf8
    $ (maybe id (:) $ lookup "href" as)
    $ (maybe id (:) $ lookup "src" as)
    []
getRefs _ = []
