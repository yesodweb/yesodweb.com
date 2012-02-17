module Handler.Root where

import Import
import Text.Blaze (unsafeByteString)
import qualified Data.ByteString as S

getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
    setTitle "Yesod Web Framework for Haskell"
    c <- liftIO $ S.readFile "content/homepage.html"
    toWidget $ unsafeByteString c
