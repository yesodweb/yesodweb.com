module Blog
    ( Blog (..)
    , Year
    , Month
    , Slug
    , Post (..)
    ) where

import Prelude
import Data.Text (Text, pack)
import Yesod.Core (PathPiece (..))
import Data.Yaml
import Control.Monad (mzero)
import Control.Applicative
import qualified Filesystem.Path.CurrentOS as F
import Data.Time
import Data.Map (Map)
import qualified Data.Map as Map

newtype Slug = Slug Text
    deriving (Read, Eq, Show, PathPiece, Ord)
type Year = Int
newtype Month = Month Int
    deriving (Read, Eq, Show, Ord, Num)
instance PathPiece Month where
    toPathPiece (Month i)
        | i < 10 && i >= 0 = pack $ '0' : show i
        | otherwise = toPathPiece i
    fromPathPiece t = do
        i <- fromPathPiece t
        if i >= 1 && i <= 12
            then Just $ Month i
            else Nothing

data Post = Post
    { postTime :: UTCTime
    , postAuthor :: Text
    , postTitle :: Text
    , postFP :: F.FilePath
    }

type AList a b = [(a, b)]
newtype Blog = Blog (Map Year (Map Month (AList Slug Post)))

instance FromJSON Post where
    parseJSON (Object o) = Post
        <$> o .: "time"
        <*> o .: "author"
        <*> o .: "title"
        <*> (F.fromText <$> o .: "path")
    parseJSON _ = mzero

instance FromJSON Blog where
    parseJSON v = do
        posts <- parseJSON v
        let tuples = map toTuple posts
        return $ Blog $ Map.unionsWith (Map.unionWith (++)) tuples
      where
        toTuple p =
            Map.singleton y $ Map.singleton m [(slug, p)]
          where
            (y', m', _) = toGregorian $ utctDay $ postTime p
            y = fromInteger y'
            m = Month m'
            slug = Slug $ either id id $ F.toText $ F.basename $ postFP p
