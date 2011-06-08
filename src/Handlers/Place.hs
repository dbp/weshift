{-# LANGUAGE OverloadedStrings #-}

module Handlers.Place where
  
import Snap.Types
import Application
import Auth (checkPlaceLogin)
import Common
import Control.Monad
import Control.Monad.Trans (liftIO)

import Data.Time.Format
import System.Locale
import qualified Data.ByteString.Char8 as B8

import State.Types
import State.Shifts
import Common
import Snap.Types
import Text.Templating.Heist
import Snap.Extension.Heist

import Auth

import Handlers.Coworkers
import Handlers.Help
import Handlers.Settings
import Handlers.Shifts
import Handlers.Messages
import Handlers.Month

placeSite :: Application ()
placeSite = do
  org <- getParam "organization"
  place <- getParam "place"
  checkPlaceLogin org place
  {-h <- fmap (getHeader "X-Requested-With") $ getRequest
  liftIO $ putStrLn $ show h-}
  route [ ("/",                       ifTop $ placeHomeH)
        , ("/month/:year/:month",     monthH)
        , ("/day/:year/:month/:day",  dayH)  
        , ("/timesheet",              timesheetH)  
        , ("/bulk",                   bulkInputH)          
        , ("/coworkers",              coworkersH)              
        , ("/help",                   helpH)              
        , ("/settings",               settingsH)              
        , ("/shift",                  shiftH)
        , ("/messages",               messagesH)
        ]

placeHomeH = do mu <- getCurrentUser
                mp <- getCurrentPlace
                nextShift <- case (mu,mp) of
                  (Just u, Just p) -> getNextShift u p
                  _ -> return Nothing
                let nextShiftSplice = spliceMBS "nextShift" $ liftM (B8.pack . (formatTime defaultTimeLocale "%-l:%M%P, %-e %-B  %Y").sStart) nextShift
                let monthSplice = [("month", monthView 2011 5)]
                heistLocal (bindSplices (nextShiftSplice ++ monthSplice)) $ renderWS "place"
                
monthH = heistLocal (bindSplices [("month", monthView 2011 5)]) $ renderWS "work/month_calendar"
dayH = renderWS "work/day_calendar"
timesheetH = renderWS "work/timesheet"
bulkInputH = renderWS "work/bulk"