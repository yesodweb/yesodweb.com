module Handler.Page where

import Import

getPageR :: [Text] -> Handler RepHtml
getPageR ps =
    returnContent "content/page" title [unsafeHtmlFormat, markdownFormat, redirectFormat] $ ContentPath ps
  where
    title
        | null ps = ""
        | otherwise = last ps
