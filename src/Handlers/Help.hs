{-# LANGUAGE OverloadedStrings #-}

module Handlers.Help where
  
import Snap.Types
import Application
import Common
import State.Types

helpH :: User -> UserPlace -> Application ()
helpH u p = renderWS "profile/help/blank"
