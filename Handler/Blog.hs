module Handler.Blog where

import Import

getBlogPostR :: Year -> Month -> Slug -> Handler RepHtml
getBlogPostR y m s = do
    (_, blog) <- runDB $ getBy404 $ UniqueBlog y m s
    returnContent [markdownFormat, htmlFormat] $ blogPath blog
