module Handler.Wiki
    ( getWikiHomeR
    , getWikiR
    ) where

import Import
import Network.HTTP.Types (status301)
import qualified Data.Text as T

getWikiHomeR :: Handler RepHtml
getWikiHomeR = redirectWith status301 $ WikiR "Home"

getWikiR :: Text -> Handler RepHtml
getWikiR path
    | path == path' =
        returnContent "yesod.wiki" path [markdownFormat] $ ContentPath [path]
    | otherwise = redirectWith status301 $ WikiR path'
  where
    path' = T.map fix path
    fix ' ' = '-'
    fix '/' = '-'
    fix c = c
