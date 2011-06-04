{-# LANGUAGE OverloadedStrings #-}

module Handlers.Place where
  
import Snap.Types
import Application
import Auth (checkPlaceLogin)
import Common

import Handlers.Coworkers
import Handlers.Help
import Handlers.Settings
import Handlers.Shift
import Handlers.Messages

placeSite :: Application ()
placeSite = do
  org <- getParam "organization"
  place <- getParam "place"
  checkPlaceLogin org place
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

placeHomeH = renderWS "place"
monthH = renderWS "work/month_calendar"
dayH = renderWS "work/day_calendar"
timesheetH = renderWS "work/timesheet"
bulkInputH = renderWS "work/bulk"