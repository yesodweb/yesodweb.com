module Handler.Wiki
    ( getWikiHomeR
    , getWikiR
    ) where

import Import
import Network.HTTP.Types (status301)
import qualified Data.Text as T

getWikiHomeR :: Handler RepHtml
getWikiHomeR = redirectWith status301 ("https://github.com/yesodweb/yesod/wiki" :: T.Text)

getWikiR :: Text -> Handler RepHtml
getWikiR path = redirectWith status301 $ "https://github.com/yesodweb/yesod/wiki/" `T.append` path
