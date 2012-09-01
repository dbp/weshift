{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------

import            Control.Monad
import            Control.Monad.Trans (liftIO)
import            Control.Applicative
import            Data.ByteString (ByteString)
import            Data.Maybe
import qualified  Data.Text as T
import            Snap.Core
import            Snap.Snaplet
import            Snap.Snaplet.Heist
import            Snap.Snaplet.Session.Backends.CookieSession
import            Snap.Util.FileServe
import            Text.Templating.Heist
import qualified  Data.Text.Encoding as TE
import            Snap.Core
import qualified  Data.Bson as B
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Time.Clock (getCurrentTime, diffUTCTime)

import Database.HDBC.PostgreSQL
import Database.HDBC
import Data.Pool

import            Text.XmlHtml (docContent)
import            Text.Blaze.Renderer.XmlHtml (renderHtml)
import            Data.List (null, sortBy, find)
import            System.Random (randomRIO)
import            Heist.Splices.Async

------------------------------------------------------------------------------
import            Application
import            Secrets (pgUser, pgPassword)
import            State.Types
import qualified  Utils as U
import            Common
import            Auth


import Handlers.Place (placeSite)
import Handlers.Account
import Handlers.Settings (activateEmail, activateDisabled)
import Handlers.Shifts (coverShiftH)
import State.Place
import Splices.Place

site = [ ("/",                      ifTop indexH)
       , ("/js",                    serveDirectory "resources/static/js")
       , ("/css",                   serveDirectory "resources/static/css")
       , ("/img",                   serveDirectory "resources/static/img")
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
  heistLocal (bindSplices [("organization-errors", blackHoleSplice)
                          ,("place-errors", blackHoleSplice)
                          ,("name-errors", blackHoleSplice)
                          ]) $ renderWS "index"


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "weshift" "An application for coordinating shifts." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "config/site-key.txt" "weshift-session" (Just 3600)
    d <- liftIO $ createPool
                        (connectPostgreSQL "hostaddr=127.0.0.1 dbname=postgres user=postgres password=pass") 
                        disconnect 
                        1
                        10
                        5
    addRoutes site
    return $ App h s d