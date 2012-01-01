module Model where

import Prelude
import Yesod
import Data.Text (Text, pack)
import Database.Persist.Quasi
import Data.Time (UTCTime)
import Import.Content (ContentPath)

newtype Slug = Slug Text
    deriving (Read, Eq, Show, PersistField, PathPiece, Ord)
type Year = Int
newtype Month = Month Int
    deriving (Read, Eq, Show, PersistField, Ord)
instance PathPiece Month where
    toPathPiece (Month i)
        | i < 10 && i >= 0 = pack $ '0' : show i
        | otherwise = toPathPiece i
    fromPathPiece t = do
        i <- fromPathPiece t
        if i >= 1 && i <= 12
            then Just $ Month i
            else Nothing

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlMkSettings, mkMigrate "migrateAll"]
    $(persistFile upperCaseSettings "config/models")
