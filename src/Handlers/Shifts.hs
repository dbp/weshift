{-# LANGUAGE OverloadedStrings #-}

module Handlers.Shifts where
  
import Snap.Types
import Application

shiftH :: Application ()
shiftH = route [ ("/add",             shiftAddH)
               , ("/edit/:id",        shiftEditH)
               , ("/delete/:id",      shiftDeleteH)
               , ("/requestoff/:id",  requestOffH)
               , ("/cover/:id",       coverH)
               ]

shiftAddH = undefined
shiftEditH = undefined
shiftDeleteH = undefined
requestOffH = undefined
coverH = undefined