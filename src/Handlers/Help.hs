{-# LANGUAGE OverloadedStrings #-}

module Handlers.Help where
  
import Snap.Types
import Application
import Common
import State.Types

helpH :: User -> UserPlace -> AppHandler ()
helpH u p = do setView u "profile" "profile.help"
               renderWS "profile/help/blank"
