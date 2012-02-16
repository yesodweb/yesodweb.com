module Handler.Blog where

import Import

getBlogPostR :: Year -> Month -> Slug -> Handler RepHtml
getBlogPostR y m s = do
    Entity _ blog <- runDB $ getBy404 $ UniqueBlog y m s
    returnContent "content" [markdownFormat, htmlFormat] $ blogPath blog
