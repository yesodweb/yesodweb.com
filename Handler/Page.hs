module Handler.Page where

import Import

getPageR :: [Text] -> Handler RepHtml
getPageR = returnContent [unsafeHtmlFormat, markdownFormat] . ContentPath . ("page":)
