{-# LANGUAGE OverloadedStrings #-}

module Handlers.Coworkers where
  
import Snap.Types
import Application

coworkersH :: Application ()
coworkersH = route [undefined]
