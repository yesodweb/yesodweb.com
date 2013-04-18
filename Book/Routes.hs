module Book.Routes where

import Prelude (IO, Maybe)
import Yesod
import Data.Text (Text)
import Data.IORef (IORef)
import Book (Book)
import Filesystem.Path (FilePath)

data BookSub = BookSub
    { bsBook :: IORef Book
    , bsRoot :: FilePath
    , bsReload :: IO ()
    , bsTitle :: Html
    , bsWarning :: Maybe Html
    }

mkYesodSubData "BookSub" [parseRoutes|
/ BookHomeR GET
/#Text ChapterR GET
/image/#Text BookImageR GET
|]
