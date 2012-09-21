{-# LANGUAGE OverloadedStrings #-}

module Render.Shifts where

-- | Boilerplate imports
import            Imports
import qualified  Data.Text as T
import qualified  Data.Text.Encoding as TE
import qualified  Data.Bson as B
import qualified  Data.Map as M
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import qualified  Text.XmlHtml as X
import qualified  Utils as U

-- | Module specific imports
import State.Shifts


renderShift :: Shift -> Splice AppHandler
renderShift (Shift id' user place start stop color units recorded recorder) = do
  req <- lift $ maybe (return Nothing) (getShiftRequest user) (if id' == "" then Nothing else Just id')
  runChildrenWith [("id", textSplice $ TE.decodeUtf8 id')
                  ,("user", textSplice $ TE.decodeUtf8 user)
                  ,("place", textSplice $ TE.decodeUtf8 place)
                  ,("date", textSplice $ renderDate start)
                  ,("start", textSplice $ renderTime start)
                  ,("stop", textSplice $ renderTime stop)
                  ,("color", textSplice $ T.pack $ show color)
                  ,("units", textSplice $ T.pack $ show units)
                  ,("recorded", textSplice $ renderTime recorded)
                  ,("recorder", textSplice $ TE.decodeUtf8 recorder)
                  ,("ifRequested", if isJust req then identitySplice else blackHoleSplice)
                  ,("notRequested", if isJust req then blackHoleSplice else identitySplice)
                  ,("reqid", textSplice $ TE.decodeUtf8 $ fromMaybe "" req)
                  ]

renderShifts :: [Shift] -> Splice AppHandler
renderShifts ss = mapSplices renderShift ss   