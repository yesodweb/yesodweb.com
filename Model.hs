module Model where

import Prelude
import Yesod
import Data.Text (Text, pack)
import Database.Persist.Quasi
import Data.Time (UTCTime)
import Import.Content (ContentPath)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith upperCaseSettings "config/models")
