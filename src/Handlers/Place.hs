{-# LANGUAGE OverloadedStrings #-}

module Handlers.Place where
  
import Snap.Types
import Application
import Auth (checkPlaceLogin)
import Common
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe, isNothing, fromJust)

import Data.Time.Format
import Data.Time.Clock
import Data.Time.Calendar
import System.Locale

import Snap.Types
import Text.Templating.Heist
import Snap.Extension.Heist

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.XmlHtml as X

import State.Types
import State.Shifts
import State.Coworkers
import State.Account
import Common

import Auth

import Handlers.Coworkers
import Handlers.Help
import Handlers.Settings
import Handlers.Shifts
import Handlers.Messages
import Handlers.Month
import Handlers.Timesheet

placeSite :: Application ()
placeSite = do
  org <- getParam "organization"
  place <- getParam "place"
  checkPlaceLogin org place
  {-h <- fmap (getHeader "X-Requested-With") $ getRequest
  liftIO $ putStrLn $ show h-}
  route [ ("/",                       ifTop $ placeHomeH)
        , ("/month/:year/:month/:day/large", monthDayLargeH)
        , ("/month/:year/:month/:day/small", monthDaySmallH)
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
                today <- liftM utctDay $ liftIO getCurrentTime
                nextShift <- case (mu,mp) of
                  (Just u, Just p) -> getNextShift u p
                  _ -> return Nothing
                let nextShiftSplice = spliceMBS "nextShift" $ liftM (B8.pack . (formatTime defaultTimeLocale "%-l:%M%P, %-e %-B  %Y").sStart) nextShift
                heistLocal (bindSplices (nextShiftSplice ++ (monthSplices today) ++ (commonSplices today))) $ renderWS "place"

monthDayLargeH = do mmonth <- getParam "month"
                    myear  <- getParam "year"
                    mday   <- getParam "day"
                    mplace <- getCurrentPlace
                    muser <- getCurrentUser
                    today <- liftM utctDay $ liftIO getCurrentTime
                    daySplice <- case (mmonth >>= maybeRead, myear >>= maybeRead, mday >>= maybeRead, mplace, muser) of
                                      (Just month, Just year, Just day, Just place, Just user) -> do
                                        shifts <- getShifts (fromGregorian year month day) (fromGregorian year month (day+1)) place
                                        return [("day", renderDay $ formatDay year month shifts [] user (emptyDayFormat (Just day)))]
                                      _ -> return []
                    heistLocal (bindSplices (daySplice ++ (commonSplices today))) $renderWS "work/month_day_large"

monthDaySmallH = do mmonth <- getParam "month"
                    myear  <- getParam "year"
                    mday   <- getParam "day"
                    mplace <- getCurrentPlace
                    muser <- getCurrentUser
                    today <- liftM utctDay $ liftIO getCurrentTime
                    daySplice <- case (mmonth >>= maybeRead, myear >>= maybeRead, mday >>= maybeRead, mplace, muser) of
                                      (Just month, Just year, Just day, Just place, Just user) -> do
                                        shifts <- getShifts (fromGregorian year month day) (fromGregorian year month (day+1)) place
                                        return [("day", renderDay $ formatDay year month shifts [] user (emptyDayFormat (Just day)))]
                                      _ -> return []
                    heistLocal (bindSplices (daySplice ++ (commonSplices today))) $ renderWS "work/month_day_small"
                                
monthH = do month <- fmap (maybeRead =<<) $ getParam "month"
            year <- fmap (maybeRead =<<) $ getParam "year"
            today <- liftM utctDay $ liftIO getCurrentTime
            let day = fromMaybe today $ liftM2 (\y m -> fromGregorian y m 1) year month
            heistLocal (bindSplices ((monthSplices day) ++ (commonSplices today))) $ renderWS "work/month_calendar"


commonSplices today = [("currYear",  textSplice $ T.pack $ show year)
                      ,("currMonth", textSplice $ T.pack $ show month)
                      ]
  where (year,month,_) = toGregorian today         
            
monthSplices day = [("monthName", textSplice $ T.pack (formatTime defaultTimeLocale "%B %Y" day))
                   ,("monthDays", monthView year month)
                   ,("nextYear",  textSplice $ T.pack $ show nextYear)
                   ,("nextMonth", textSplice $ T.pack $ show nextMonth)
                   ,("prevYear",  textSplice $ T.pack $ show prevYear)
                   ,("prevMonth", textSplice $ T.pack $ show prevMonth)
                   ,("day", identitySplice) -- this is to allow the same template to be used in other ways
                   ]
  where (year,month,_) = toGregorian day
        (nextYear,nextMonth,_) = toGregorian $ addGregorianMonthsClip 1 day
        (prevYear,prevMonth,_) = toGregorian $ addGregorianMonthsClip (-1) day
  
dayH = do
  today <- liftM utctDay $ liftIO getCurrentTime
  heistLocal (bindSplices (commonSplices today)) $ renderWS "work/day_calendar"
  
timesheetH = do
  mplace <- getCurrentPlace
  when (isNothing mplace) $ redirect "/"
  muser <- getCurrentUser
  when (isNothing muser) $ redirect "/"
  let place = fromJust mplace
  
  
  targetUser <- getParam "user"
  mstart <- getParam "start"
  mstop <- getParam "stop"

  user' <- case targetUser of
            Just tU -> if pFac place then fmap mkUser (getUser tU) else return muser
            Nothing -> return muser
  when (isNothing user') $ redirect "/"
  let user = fromJust user'
  
  coworkers <- getCoworkers user place
  let coworkersSplice = [("timesheetCoworkers", renderTSCoworkers user coworkers)]
  
  today <- liftM utctDay $ liftIO getCurrentTime
  
  timesheetSplice <- case (mstart >>= parseWSDate,mstop >>= parseWSDate) of
                        (Just start, Just stop) -> getTimesheet place user start stop
                        _ -> getTimesheet place user (addDays (-14) today) today
  
  heistLocal (bindSplices (commonSplices today ++ coworkersSplice ++ timesheetSplice)) $ renderWS "work/timesheet"

    where renderTSCoworkers self coworkers = mapSplices (renderTSCoworker self) (self : coworkers)
          renderTSCoworker self u = runChildrenWithText [ ("userId",   TE.decodeUtf8 $ uId u)
                                                        , ("userName", TE.decodeUtf8 $ uName u)
                                                        , ("selected", if u == self then "selected='selected'" else "")
                                                        ]

bulkInputH = do
    today <- liftM utctDay $ liftIO getCurrentTime
    heistLocal (bindSplices (commonSplices today)) $ renderWS "work/bulk"