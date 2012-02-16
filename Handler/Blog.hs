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
import Data.Maybe (listToMaybe)

getBlogR :: Handler ()
getBlogR = do
    Blog blog <- ywBlog <$> getYesod
    newest <- maybe notFound return $ listToMaybe $ do
        (year, x) <- take 1 $ reverse $ sortBy (comparing fst) $ Map.toList blog
        (month, y) <- take 1 $ reverse $ sortBy (comparing fst) $ Map.toList x
        (slug, _) <- take 1 y
        return $ BlogPostR year month slug
    redirect newest

getBlogPostR :: Year -> Month -> Slug -> Handler RepHtml
getBlogPostR y m s = do
    Blog blog <- ywBlog <$> getYesod
    blog' <- maybe notFound return $ Map.lookup y blog
    blog'' <- maybe notFound return $ Map.lookup m blog'
    post <- maybe notFound return $ lookup s blog''
    content <- liftIO $ S.readFile $ F.encodeString $ blogRoot F.</> postFP post
    defaultLayout $ do
        setTitle $ toHtml $ postTitle post
        let rev :: Ord k => Map.Map k v -> [(k, v)]
            rev = reverse . sortBy (comparing fst) . Map.toList
        [whamlet|
<nav #archive>
    <ul>
        $forall (year, x) <- rev blog
            <li>
                <span>#{year}
                <ul>
                    $forall (month, y) <- rev x
                        <li>
                            <span>#{pretty month}
                            <ul>
                                $forall (slug, post) <- y
                                    <li>
                                        <a href=@{BlogPostR year month slug}>#{postTitle post}
<article>
    ^{toWidget $ unsafeByteString content}
|]
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
