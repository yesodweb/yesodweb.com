module Book.Routes where

import RIO (IO, Maybe, FilePath, Text, IORef, SomeException, Either)
import Yesod
import Book (Book)

data BookSub = BookSub
    { bsBook :: IORef (Either SomeException Book)
    , bsRoot :: FilePath
    , bsReload :: IO ()
    , bsTitle :: Html
    , bsWarning :: Maybe Html
    , bsBranch :: Text
    }

mkYesodSubData "BookSub" [parseRoutes|
/ BookHomeR GET
/#Text ChapterR GET
/image/#Text BookImageR GET
|]
