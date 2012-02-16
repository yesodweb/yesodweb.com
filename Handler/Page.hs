module Handler.Page where

import Import

getPageR :: [Text] -> Handler RepHtml
getPageR = returnContent "content" [unsafeHtmlFormat, markdownFormat] . ContentPath . ("page":)
