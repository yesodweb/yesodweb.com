module Handler.Blog
    ( getBlogR
    , getBlogPostR
    ) where

import Import
import qualified Data.Map as Map
import qualified Data.ByteString as S
import qualified Filesystem.Path.CurrentOS as F
import Text.Blaze (unsafeByteString)
import Settings (blogRoot)
import Data.List (sortBy)
import Data.Ord (comparing)

getBlogR :: Handler ()
getBlogR = getNewestBlog >>= redirect . fst

getBlogPostR :: Year -> Month -> Slug -> Handler RepHtml
getBlogPostR y m s = do
    Blog blog <- ywBlog <$> getYesod
    blog' <- maybe notFound return $ Map.lookup y blog
    blog'' <- maybe notFound return $ Map.lookup m blog'
    post <- maybe notFound return $ lookup s blog''
    content <- liftIO $ S.readFile $ F.encodeString $ blogRoot F.</> postFP post
    let currYear y' = y == y'
        currMonth y' m' = y == y' && m == m'
        currPost y' m' s' = y == y' && m == m' && s == s'
    defaultLayout $ do
        setTitle $ toHtml $ postTitle post
        let rev :: Ord k => Map.Map k v -> [(k, v)]
            rev = reverse . sortBy (comparing fst) . Map.toList
        $(widgetFile "blog")
  where
    pretty 1 = "January"
    pretty 2 = "February"
    pretty 3 = "March"
    pretty 4 = "April"
    pretty 5 = "May"
    pretty 6 = "June"
    pretty 7 = "July"
    pretty 8 = "August"
    pretty 9 = "September"
    pretty 10 = "October"
    pretty 11 = "November"
    pretty 12 = "December" :: Text
    pretty _ = "Some new month"
