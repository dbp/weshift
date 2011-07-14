{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Handlers.Place where
  
import Snap.Types
import Application
import Common
import Control.Monad
import "mtl" Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe, isNothing, fromJust, mapMaybe)

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
import Handlers.Day
import Handlers.Timesheet
import Handlers.Bulk

placeSite :: Application ()
placeSite = do
  orgName <- getParam "organization"
  placeName <- getParam "place"
  {-h <- fmap (getHeader "X-Requested-With") $ getRequest
  liftIO $ putStrLn $ show h-}
  route [ ("/",                               ifTop $ checkPlaceLogin orgName placeName placeHomeH)
        , ("/month/:year/:month/:day/large",  checkPlaceLoginAsync orgName placeName monthDayLargeH)
        , ("/month/:year/:month/:day/small",  checkPlaceLoginAsync orgName placeName monthDaySmallH)
        , ("/month/:year/:month",             checkPlaceLoginAsync orgName placeName monthH)
        , ("/day/:year/:month/:day",          checkPlaceLoginAsync orgName placeName dayH)
        , ("/timesheet",                      checkPlaceLoginAsync orgName placeName timesheetH)
        , ("/bulk",                           checkPlaceLoginAsync orgName placeName bulkInputH)
        , ("/coworkers",                      checkPlaceLoginAsync orgName placeName coworkersH)
        , ("/help",                           checkPlaceLoginAsync orgName placeName helpH)
        , ("/settings",                       checkPlaceLoginAsync orgName placeName settingsH)
        , ("/shift",                          checkPlaceLoginAsync orgName placeName shiftH)
        , ("/messages",                       checkPlaceLoginAsync orgName placeName messagesH)
        , ("/blank",                          checkPlaceLoginAsync orgName placeName (\_ _ -> renderWS "profile/blank"))
        ]

placeHomeH u p = do today <- liftM utctDay $ liftIO getCurrentTime
                    nextShift <- getNextShift u p
                    let nextShiftSplice = spliceMBS "nextShift" $ Just $ fromMaybe "No Next Shift" $ liftM (B8.pack . (formatTime defaultTimeLocale "%-l:%M%P, %-e %-B  %Y").sStart) nextShift
                    workers <- getWorkers p
                    let coworkers = filter ((/= (uId u)) . uId) workers
                    let monthday = maybe today (\(y,m) -> fromGregorian y m 1) savedMonth
                    let dayday = maybe today (\(y,m,d) -> fromGregorian y m d) savedDay
                    shifts <- getShifts dayday (addDays 1 dayday) p
                    timesheetSplice <- getTimesheet p u (addDays (-14) today) today
                    emails <- getUserEmails u
                    let emailsSplice = [("emails", renderEmails emails)]
                    
                    heistLocal (bindSplices (nextShiftSplice ++ (commonSplices today) ++ (monthSplices u p monthday) ++ (coworkersSplice coworkers) ++ (daySplices u p workers shifts dayday) ++ timesheetSplice ++ [("timesheetCoworkers", renderTSCoworkers u coworkers)] ++ emailsSplice)) $ renderWS "place"
      where mList Nothing = []
            mList (Just xs) = xs
            monthV = fmap ((T.splitOn ".") . TE.decodeUtf8) $ getView u "work.month."
            savedMonth = 
              case map TE.encodeUtf8 (mList monthV) of
                (y:m:[]) -> do month <- maybeRead m
                               year <- maybeRead y
                               return (year,month)
                _ -> Nothing
            dayV = fmap ((T.splitOn ".") . TE.decodeUtf8) $ getView u "work.day."
            savedDay = 
              case map TE.encodeUtf8 (mList dayV) of
                (y:m:d:[]) -> do year <- maybeRead y
                                 month <- maybeRead m
                                 day <- maybeRead d
                                 return (year,month,day)
                _ -> Nothing

monthDayLargeH user place = do mmonth <- getParam "month"
                               myear  <- getParam "year"
                               mday   <- getParam "day"
                               (day,daySplice) <- 
                                    case (mmonth >>= maybeRead, myear >>= maybeRead, mday >>= maybeRead) of
                                                 (Just month, Just year, Just day) -> dayLargeSplices place user (year, month, day)
                                                 _ -> do
                                                   today <- liftM utctDay $ liftIO getCurrentTime
                                                   return (today,[])
                               heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_large"


monthDaySmallH user place = do mmonth <- getParam "month"
                               myear  <- getParam "year"
                               mday   <- getParam "day"
                               (day, daySplice) <- 
                                  case (mmonth >>= maybeRead, myear >>= maybeRead, mday >>= maybeRead) of
                                    (Just month, Just year, Just day) -> do
                                      let d = (fromGregorian year month day)
                                      shifts <- getShifts d (addDays 1 d) place
                                      return (d,[("day", renderDay $ formatDay year month shifts [] user (emptyDayFormat (Just day)))])
                                    _ -> do 
                                      today <- liftM utctDay $ liftIO getCurrentTime
                                      return (today, [])
                               heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_small"
                                
monthH u p = do month <- fmap (maybeRead =<<) $ getParam "month"
                year <- fmap (maybeRead =<<) $ getParam "year"
                today <- liftM utctDay $ liftIO getCurrentTime
                let day = fromMaybe today $ liftM2 (\y m -> fromGregorian y m 1) year month
                setView u "work" (TE.encodeUtf8 (T.concat ["work.month" 
                                                          ,(maybe "" (T.append "." . T.pack . show ) year)
                                                          ,(maybe "" (T.append "." . T.pack . show) month)]))
                heistLocal (bindSplices ((monthSplices u p day) ++ (commonSplices day))) $ renderWS "work/month_calendar"

       
            
monthSplices u p day = [("monthName", textSplice $ T.pack (formatTime defaultTimeLocale "%B %Y" day))
                       ,("monthDays", monthView p u year month)
                       ,("mNextYear",  textSplice $ T.pack $ show nextYear)
                       ,("mNextMonth", textSplice $ T.pack $ show nextMonth)
                       ,("mCurrYear",  textSplice $ T.pack $ show year)
                       ,("mCurrMonth", textSplice $ T.pack $ show month)
                       ,("mPrevYear",  textSplice $ T.pack $ show prevYear)
                       ,("mPrevMonth", textSplice $ T.pack $ show prevMonth)
                       ,("day", identitySplice) -- this is to allow the same template to be used in other ways
                       ]
  where (year,month,_) = toGregorian day
        (nextYear,nextMonth,_) = toGregorian $ addGregorianMonthsClip 1 day
        (prevYear,prevMonth,_) = toGregorian $ addGregorianMonthsClip (-1) day
  
dayH u p = do
  day <- fmap (maybeRead =<<) $ getParam "day"
  month <- fmap (maybeRead =<<) $ getParam "month"
  year <- fmap (maybeRead =<<) $ getParam "year"
  today <- liftM utctDay $ liftIO getCurrentTime
  let curday = fromMaybe today $ liftM3 fromGregorian year month day
  setView u "work" (TE.encodeUtf8 (T.concat ["work.day" 
                                            ,(maybe "" (T.append "." . T.pack . show) year)
                                            ,(maybe "" (T.append "." . T.pack . show) month)
                                            ,(maybe "" (T.append "." . T.pack . show) day)
                                            ]))
  shifts <- getShifts curday (addDays 1 curday) p
  workers <- getWorkers p
  heistLocal (bindSplices ((commonSplices curday) ++ (daySplices u p workers shifts curday))) $ renderWS "work/day_calendar"


timesheetH user place = do
  targetUser <- getParam "user"
  mstart <- getParam "start"
  mstop <- getParam "stop"
  
  user' <- fmap fromJust $ case targetUser of
            Just tU -> if pFac place then fmap mkUser (getUser tU) else return $ Just user
            Nothing -> return $ Just user
            
  coworkers <- getCoworkers user' place
  let coworkersSplice = [("timesheetCoworkers", renderTSCoworkers user' coworkers)]
  
  today <- liftM utctDay $ liftIO getCurrentTime
  
  timesheetSplice <- case (mstart >>= parseWSDate,mstop >>= parseWSDate) of
                        (Just start, Just stop) -> getTimesheet place user' start stop
                        _ -> getTimesheet place user' (addDays (-14) today) today
  
  setView user "work" "work.timesheet"

  heistLocal (bindSplices (commonSplices today ++ coworkersSplice ++ timesheetSplice)) $ renderWS "work/timesheet"

