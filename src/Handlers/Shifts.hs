{-# LANGUAGE OverloadedStrings #-}

module Handlers.Shifts where
  
import Text.Templating.Heist
import qualified Data.Text.Encoding as TE
  
import Snap.Types
import Application
import State.Types
import Common

shiftH :: User -> UserPlace -> Application ()
shiftH u p = route [ ("/add",             shiftAddH)
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

renderShift :: Shift -> Splice Application
renderShift (Shift id' user place start stop recorded recorder) =
  runChildrenWithText [("id", TE.decodeUtf8 id')
                      ,("user", TE.decodeUtf8 user)
                      ,("place", TE.decodeUtf8 place)
                      ,("start", renderTime start)
                      ,("stop", renderTime stop)
                      ,("recorded", renderTime recorded)
                      ,("recorder", TE.decodeUtf8 recorder)
                      ]

renderShifts :: [Shift] -> Splice Application
renderShifts ss = mapSplices renderShift ss 