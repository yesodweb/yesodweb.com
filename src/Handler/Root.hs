module Handler.Root where

import Import
import Text.Blaze.Html (unsafeByteString)
import qualified Data.ByteString as S
import Text.Hamlet (hamletFile)
import Yesod.AtomFeed (atomLink)

getRootR :: Handler Html
getRootR = do
    c <- liftIO $ S.readFile "content/homepage.html"
    let widget = do
            setTitle "Yesod Web Framework for Haskell"
            atomLink FeedR "Yesod Web Framework Blog"
            $(widgetFile "normalize")
            $(widgetFile "homepage")
            $(widgetFile "mobile")
            toWidget $ unsafeByteString c
    pc <- widgetToPageContent widget
    (blogLink, post) <- getNewestBlog
    withUrlRenderer $(hamletFile "templates/homepage-wrapper.hamlet")
