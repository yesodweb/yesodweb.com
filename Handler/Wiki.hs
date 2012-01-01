module Handler.Wiki where

import Import

getWikiR :: [Text] -> Handler RepHtml
getWikiR = returnContent [markdownFormat, htmlFormat] . ContentPath . ("wiki":)

