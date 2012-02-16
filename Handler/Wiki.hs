module Handler.Wiki
    ( getWikiHomeR
    , getWikiR
    ) where

import Import
import Network.HTTP.Types (status301)

getWikiHomeR :: Handler RepHtml
getWikiHomeR = redirectWith status301 $ WikiR "Home"

getWikiR :: Text -> Handler RepHtml
getWikiR path =
    returnContent "yesod.wiki" [markdownFormat] $ ContentPath [path]
