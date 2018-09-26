module Handler.Wiki
    ( getWikiHomeR
    , getWikiR
    ) where

import Import
import qualified Data.Text as T

getWikiHomeR :: Handler Html
getWikiHomeR = redirect ("https://github.com/yesodweb/yesod-cookbook" :: T.Text)

getWikiR :: Text -> Handler Html
getWikiR path = redirect $ T.concat
    [ "https://github.com/yesodweb/yesod-cookbook/blob/master/cookbook/"
    , path
    , ".md"
    ]
