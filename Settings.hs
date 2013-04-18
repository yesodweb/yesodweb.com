-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings
    ( widgetFile
    , staticRoot
    , staticDir
    , Extra (..)
    , parseExtra
    , blogRoot
    , Author (..)
    ) where

import Prelude
import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Yesod.Default.Config
import qualified Yesod.Default.Util
import Data.Default (def)
import Data.Text (Text)
import Data.Yaml
import Control.Applicative
import qualified Filesystem.Path as F
import Data.Map (Map)
import Control.Monad (mzero)

blogRoot :: F.FilePath
blogRoot = "content/blog"

-- Static setting below. Changing these requires a recompile

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticDir :: FilePath
staticDir = "static"

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in Foundation.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in Foundation.hs
staticRoot :: AppConfig DefaultEnv x ->  Text
staticRoot _conf =
#if DEVELOPMENT
    [st|#{appRoot _conf}/static|]
#else
    "http://static.yesodweb.com"
#endif


-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
#if DEVELOPMENT
widgetFile = Yesod.Default.Util.widgetFileReload def
#else
widgetFile = Yesod.Default.Util.widgetFileNoReload def
#endif

data Extra = Extra

data Author = Author
    { authorName :: Text
    , authorEmail :: Text
    }

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = pure Extra

instance FromJSON Author where
    parseJSON (Object o) = Author
        <$> o .: "name"
        <*> o .: "email"
    parseJSON _ = mzero
