{-# LANGUAGE OverloadedStrings #-}

module Render.Shifts where

-- | Boilerplate imports
import qualified Data.Bson          as B
import qualified Data.Map           as M
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import           Imports
import qualified Text.XmlHtml       as X
import qualified Utils              as U

-- | Module specific imports
import           State.Shifts


renderShift :: Shift -> Splice AppHandler
renderShift (Shift id' user place start stop color units recorded
                   recorder deadline deadlineDone desc claims) = do
  req <- lift $ maybe (return Nothing) (getShiftRequest user) (if id' == "" then Nothing else Just id')
  runChildrenWith $ do "id" ## textSplice id'
                       "user" ## textSplice user
                       "place" ## textSplice place
                       "date" ## textSplice $ renderDate start
                       "start" ## textSplice $ renderTime start
                       "stop" ## textSplice $ renderTime stop
                       "color" ## textSplice $ T.pack $ show color
                       "units" ## textSplice $ T.pack $ show units
                       "recorded" ## textSplice $ renderTime recorded
                       "recorder" ## textSplice recorder
                       "ifRequested" ## if isJust req then identitySplice else blackHoleSplice
                       "notRequested" ## if isJust req then blackHoleSplice else identitySplice
                       "reqid" ## textSplice $ fromMaybe "" req
                       "isDeadline" ## booleanSplice deadline
                       "notDeadline" ## booleanSplice (not deadline)
                       "isDeadlineDone" ## booleanSplice (deadlineDone || (not deadline))
                       "notDeadlineDone" ## booleanSplice ((not deadlineDone) || (not deadline))
                       "description" ## textSplice desc
                       "hasClaims" ## booleanSplice claims
                       "noClaims" ## booleanSplice (not claims)


renderShifts :: [Shift] -> Splice AppHandler
renderShifts ss = mapSplices renderShift ss
