{-# LANGUAGE OverloadedStrings #-}

module Handlers.Help where
  
import Snap.Types
import Application

helpH :: Application ()
helpH = route [undefined]
