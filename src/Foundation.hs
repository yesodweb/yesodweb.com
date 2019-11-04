{-# OPTIONS_GHC -O0 #-}
module Foundation
    ( YesodWeb (..)
    , Route (..)
    , resourcesYesodWeb
    , Handler
    , Widget
    , Form
    , module X
    , license
    ) where

import           Blog                 as X
import           Book.Routes
import           Data.IORef           (IORef)
import           Data.Map             (Map)
import           Data.Text            (Text)
import           Prelude
import           Settings             as X (Extra (..), widgetFile)
import qualified Settings
import           Settings.StaticFiles
import           Text.Hamlet          (hamletFile)
import           Yesod                as X hiding (Route)
import           Yesod.AtomFeed       (atomLink)
import           Yesod.Default.Config
import           Yesod.Form.Jquery    (YesodJquery)
import           Yesod.Static

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data YesodWeb = YesodWeb
    { settings  :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , getAssets :: Static
    , ywBlog    :: IORef Blog
    , ywAuthors :: IORef (Map Text Settings.Author)
    , getBook12 :: BookSub
    , getBook11 :: BookSub
    , getBook14 :: BookSub
    , getBook16 :: BookSub
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype YesodWebRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route YesodWeb = YesodWebRoute
-- * Creates the value resourcesYesodWeb which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- YesodWeb. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the YesodWebRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "YesodWeb" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm Handler (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod YesodWeb where
    approot = guessApproot

    makeSessionBackend _ = return Nothing

    defaultLayout widget = do
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            atomLink FeedR "Yesod Web Framework Blog"
            addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js"
            $(widgetFile "normalize")
            $(widgetFile "highlight")
            $(widgetFile "default-layout")
            $(widgetFile "mobile")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.

    -- Disabled to allow for horizontal scaling
    --addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    jsLoader _ = BottomOfBody

license :: Html
license =
    [shamlet|
        <footer #license style="text-align:center;font-size:0.8em">
            All content on this site is available under the
            <a href=http://creativecommons.org/licenses/by/4.0/>Creative Commons Attribution 4.0 International License#
            .
            <br>
            Content outside the book section is available under
            <a href=https://raw.github.com/yesodweb/yesodweb.com/master/LICENSE>
                the MIT license
            as well.
    |]

instance YesodJquery YesodWeb
