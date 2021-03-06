{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------

-- | Boilerplate imports
import qualified Data.Bson                                   as B
import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Char8                       as B8
import qualified Data.Map                                    as M
import qualified Data.Text                                   as T
import qualified Data.Text.Encoding                          as TE
import           Imports
import qualified Text.XmlHtml                                as X
import qualified Utils                                       as U

-- | Module specific imports
import           Data.Pool
import           Heist.Splices.Async
import           Snap.Less
import           Snap.Snaplet.Session.Backends.CookieSession

import           DigestiveAdaptor                            (digestiveAdaptorSplices)
import           Secrets                                     (dbName,
                                                              pgPassword,
                                                              pgUser)


import           Handlers.Account
import           Handlers.Place                              (placeSite)
import           Handlers.Settings                           (activateDisabled,
                                                              activateEmail)
import           Handlers.Shifts                             (coverShiftH)

import           Forms.Account

import           State.Place

site = [ ("/",                      ifTop indexH)
       , ("/js",                    serveDirectory "static/js")
       , ("/css",                   withLess renderLess)
       , ("/css",                   serveDirectory "static/css")
       , ("/img",                   serveDirectory "static/img")
       , ("/activate/account",      activateAccountH)
       , ("/activate/email",        activateEmail)
       , ("/activate/disabled",     activateDisabled)
       , ("/cover",                 coverShiftH)
       , ("/:organization/:place",  placeSite)
       , ("/signup",                signupH)
       , ("/login",                 method GET $ loginGetH ())
       , ("/login",                 method POST $ loginPostH loginGetH redirTo)
       , ("/logout",                method GET $ logoutH redirTo)
       ]

indexH = do
  u <- getCurrentUser
  (view, _) <- wsForm (signupForm u)
  heistLocal (bindDigestiveSplices view) $ renderWS "index"


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "weshift" "An application for coordinating shifts." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "config/site-key.txt" "weshift-session" (Just 3600)
    d <- liftIO $ createPool
                        (connectPostgreSQL ("hostaddr=127.0.0.1 dbname=" ++ dbName ++ " user=" ++ pgUser ++ " password=" ++ pgPassword))
                        disconnect
                        1
                        10
                        5
    l <- liftIO $ newLessDirectory' "snaplets/lesscss/stylesheets"
    addRoutes site
    modifyHeistState (bindSplices digestiveAdaptorSplices)
    return $ App h s d l
