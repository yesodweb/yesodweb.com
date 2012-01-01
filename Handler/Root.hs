module Handler.Root where

import Import

getRootR :: Handler RepHtml
getRootR = returnContent [unsafeHtmlFormat] $ ContentPath ["homepage"]
