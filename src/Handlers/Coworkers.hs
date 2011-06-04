{-# LANGUAGE OverloadedStrings #-}

module Handlers.Coworkers where
  
import Snap.Types
import Application
import Common

coworkersH :: Application ()
coworkersH = renderWS "profile/coworkers/blank"
