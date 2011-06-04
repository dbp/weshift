{-# LANGUAGE OverloadedStrings #-}

module Handlers.Settings where
  
import Snap.Types
import Application
import Common

settingsH :: Application ()
settingsH = route [ ("/",         ifTop $ renderWS "profile/usersettings/blank")
                  , ("/name",     changeNameH)
                  , ("/password", changePasswordH)
                  , ("/remove",   removeAccountH)
                  , ("/email",    emailH)
                  ]
                  
changeNameH = undefined
changePasswordH = renderWS "profile/usersettings/password"
removeAccountH = undefined
emailH = undefined