module Handler.Root where

import Import

getRootR :: Handler RepHtml
getRootR = returnContent "content" [unsafeHtmlFormat] $ ContentPath ["homepage"]
