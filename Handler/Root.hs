module Handler.Root where

import Import
import Text.Blaze (unsafeByteString)
import qualified Data.ByteString as S
import Text.Hamlet (hamletFile)

getRootR :: Handler RepHtml
getRootR = do
    c <- liftIO $ S.readFile "content/homepage.html"
    let widget = do
            setTitle "Yesod Web Framework for Haskell"
            $(widgetFile "normalize")
            $(widgetFile "homepage")
            toWidget $ unsafeByteString c
    pc <- widgetToPageContent widget
    (blogLink, post) <- getNewestBlog
    hamletToRepHtml $(hamletFile "templates/homepage-wrapper.hamlet")
