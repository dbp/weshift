{-# LANGUAGE OverloadedStrings #-}

module Handlers.Help where
  
import Snap.Types
import Application
import Common

helpH :: Application ()
helpH = renderWS "profile/help/blank"
