module Handler.Root where

import Import

getRootR :: Handler RepHtml
getRootR = defaultLayout $(widgetFile "homepage")
