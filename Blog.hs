{-# LANGUAGE ViewPatterns #-}
module Blog
    ( Blog (..)
    , Year
    , Month (..)
    , Slug
    , Post (..)
    , filterBlog
    ) where

import Prelude
import Data.Text (Text, pack)
import Yesod.Core (PathPiece (..))
import Data.Yaml
import Control.Monad (mzero)
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
    , postRaw :: Bool
    -- ^ don't apply any template to the raw page
    }

type AList a b = [(a, b)]
newtype Blog = Blog (Map Year (Map Month (AList Slug Post)))

filterBlog :: UTCTime -> Blog -> Blog
filterBlog now (Blog years) =
    Blog $ Map.mapMaybeWithKey goYear years
  where
    (fromInteger -> curryear, currmonth, _) = toGregorian $ utctDay now

    goYear year m =
        case compare curryear year of
            GT -> Just m
            LT -> Nothing
            EQ ->
                let m' = Map.mapMaybeWithKey goMonth m
                 in if Map.null m' then Nothing else Just m'

    goMonth (Month month) alist =
        case compare currmonth month of
            GT -> Just alist
            LT -> Nothing
            EQ ->
                let alist' = filter goPair alist
                 in if null alist' then Nothing else Just alist'

    goPair (_, post) = postTime post <= now

instance FromJSON Post where
    parseJSON (Object o) = Post
        <$> o .: "time"
        <*> o .: "author"
        <*> o .: "title"
        <*> (F.fromText <$> o .: "path")
        <*> o .:? "raw" .!= False
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
