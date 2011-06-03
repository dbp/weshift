{-# LANGUAGE OverloadedStrings #-}

module Handlers.Settings where
  
import Snap.Types
import Application

settingsH :: Application ()
settingsH = route [ ("/name",     changeNameH)
                  , ("/password", changePasswordH)
                  , ("/remove",   removeAccountH)
                  , ("/email",    emailH)
                  ]
                  
changeNameH = undefined
changePasswordH = undefined
removeAccountH = undefined
emailH = undefined