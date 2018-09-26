module Handler.Page where

import Import

getPageR :: [Text] -> Handler Html
getPageR ps =
    returnContent "content/page" title [unsafeHtmlFormat, markdownFormat, redirectFormat] $ ContentPath ps
  where
    title = fromMaybe "" $ lastMay ps

    lastMay [] = Nothing
    lastMay [x] = Just x
    lastMay (_:rest) = lastMay rest
