{-# LANGUAGE OverloadedStrings #-}

module Handlers.Account where
  
import Snap.Types
import Common
import Application


newSessionH :: a -> Application ()
newSessionH = \_ -> renderWS "login"

signupH = undefined