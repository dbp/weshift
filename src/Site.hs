{-# LANGUAGE OverloadedStrings #-}

module Site
  ( site
  ) where

import            Control.Applicative
import            Control.Monad
import            Control.Monad.Trans (liftIO)
import            Data.Maybe
import qualified  Data.Text.Encoding as TE
import qualified  Data.Text as T
import            Snap.Extension.Heist
import            Snap.Extension.Session.CookieSession
import            Snap.Util.FileServe
import            Snap.Types
import            Text.Templating.Heist
import            Snap.Auth
import            Snap.Auth.Handlers
import qualified  Data.Bson as B
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Time.Clock (getCurrentTime, diffUTCTime)

import            Text.Digestive.Types
import            Text.Digestive.Blaze.Html5
import            Text.Digestive.Forms.Snap
import            Text.Digestive.Validate

import            Text.Blaze (Html)
import            Text.XmlHtml (docContent)
import            Text.Blaze.Renderer.XmlHtml (renderHtml)
import            Data.List (null, sortBy, find)
import            System.Random (randomRIO)
import            Heist.Splices.Async

import            Application
import            State
import            Form
import qualified  Utils as U
import            Common
import            Auth

import Handlers.Place (placeSite)
import Handlers.Account

formSplice :: FormHtml Html -> Splice Application
formSplice = return . docContent . renderHtml . fst . renderFormHtml

site :: Application ()                 
site = route [ ("/",                      render "index")
             , ("/:organization/:place",  placeSite)
             , ("/signup",                signupH)
             , ("/login",                 method GET $ newSessionH ())
             , ("/login",                 method POST $ loginH newSessionH redirTo)
             , ("/logout",                method GET $ logoutHandler redirTo)
             ]
       <|> serveDirectory "resources/static"

{-  This is going to work by having just about everything via ajax. Digestive-functors via ajax. 
    So all forms are fetched when needed, and then posted and redisplayed if errors. 
-}
