{-# LANGUAGE OverloadedStrings #-}

module Handlers.Place where

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
import State.Coworkers
import State.Account

import Handlers.Coworkers
import Handlers.Help
import Handlers.Settings
import Handlers.Shifts
import Handlers.Messages
import Handlers.Timesheet
import Handlers.Bulk
import Handlers.Messages

import Render.Calendar
import Render.Timesheet

placeSite :: AppHandler ()
placeSite = do
  orgName <- getParam "organization"
  placeName <- getParam "place"
  {-h <- fmap (getHeader "X-Requested-With") $ getRequest
  liftIO $ putStrLn $ show h-}
  route $ [ ("/",                               ifTop $ checkPlaceLogin orgName placeName placeHomeH) ] ++
          (map (\(a,b) -> (a, checkPlaceLoginAsync orgName placeName b))
            [ ("/month/:year/:month/:day/large", monthDayLargeH)
            , ("/month/:year/:month/:day/small", monthDaySmallH)
            , ("/month/:year/:month",            monthH)
            , ("/day/:year/:month/:day",         dayH)
            , ("/timesheet",                     timesheetH)
            , ("/bulk",                          bulkInputH)
            , ("/coworkers",                     coworkersH)
            , ("/help",                          helpH)
            , ("/settings",                      settingsH)
            , ("/shift",                         shiftH)
            , ("/messages",                      messagesH)
            , ("/blank",                         (\_ _ -> renderWS "profile/blank"))])

placeHomeH u p = do today <- liftM utctDay $ liftIO getCurrentTime
                    nextShift <- getNextShift u p
                    let nextShiftSplice = spliceMBS "nextShift" $ Just $ fromMaybe "No Next Shift" $ liftM (B8.pack . (formatTime defaultTimeLocale "%-l:%M%P, %-e %-B  %Y").sStart) nextShift
                    workers <- getWorkers p
                    let coworkers = filter ((/= (uId u)) . uId) workers
                    let monthday = maybe today (\(y,m,d') -> fromGregorian y m (fromMaybe 1 d')) savedMonth
                    let dayday = maybe today (\(y,m,d) -> fromGregorian y m d) savedDay
                    shifts <- getShifts dayday (addDays 1 dayday) p
                    timesheetSplice <- getTimesheet p u (addDays (-14) today) today
                    emails <- getUserEmails u
                    let showDay = case savedMonth of
                                    Just (_,_,Just d) -> Just d
                                    _ -> Nothing
                    let emailsSplice = [("emails", renderEmails emails)]
                    messagesSplice <- messagesPageSplices p 1
                    heistLocal (bindSplices (nextShiftSplice ++ (commonSplices today) ++ (monthSplices u p monthday showDay) ++ 
                                             (coworkersSplice coworkers) ++ (daySplices u p workers shifts dayday) ++ 
                                             timesheetSplice ++ [("timesheetCoworkers", renderTSCoworkers u coworkers)] ++ 
                                             emailsSplice ++ messagesSplice)) $ renderWS "place"
      where mList Nothing = []
            mList (Just xs) = xs
            monthV = fmap ((T.splitOn ".") . TE.decodeUtf8) $ getView u "work.month."
            savedMonth = 
              case map TE.encodeUtf8 (mList monthV) of
                (y:m:md) -> do month <- maybeRead m
                               year <- maybeRead y
                               let d = case md of
                                        (d':[]) -> maybeRead d' 
                                        _ -> Nothing
                               return (year,month,d)
                _ -> Nothing
            dayV = fmap ((T.splitOn ".") . TE.decodeUtf8) $ getView u "work.day."
            savedDay = 
              case map TE.encodeUtf8 (mList dayV) of
                (y:m:d:[]) -> do year <- maybeRead y
                                 month <- maybeRead m
                                 day <- maybeRead d
                                 return (year,month,day)
                _ -> Nothing

monthDaySmallH user place = do mmonth <- getParam "month"
                               myear  <- getParam "year"
                               mday   <- getParam "day"
                               (day, daySplice) <- 
                                  case (mmonth >>= maybeRead, myear >>= maybeRead, mday >>= maybeRead) of
                                    (Just month, Just year, Just day) -> do
                                      let d = (fromGregorian year month day)
                                      shifts <- getShifts d (addDays 1 d) place
                                      setView user "work" (TE.encodeUtf8 (T.concat ["work.month." 
                                                                                   ,T.pack $ show year
                                                                                   ,"."
                                                                                   ,T.pack $ show month
                                                                                   ]))
                                      
                                      return (d,[("day", renderDay user Nothing [] $ formatDay year month shifts [] user (emptyDayFormat (Just day)))])
                                    _ -> do 
                                      today <- liftM utctDay $ liftIO getCurrentTime
                                      return (today, [])
                               heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_small"

monthDayLargeH user place = do mmonth <- getParam "month"
                               myear  <- getParam "year"
                               mday   <- getParam "day"
                               (day,daySplice) <- 
                                    case (mmonth >>= maybeRead, myear >>= maybeRead, mday >>= maybeRead) of
                                                 (Just month, Just year, Just day) -> do
                                                   setView user "work" (TE.encodeUtf8 (T.concat ["work.month" 
                                                                                                ,"."
                                                                                                ,T.pack $ show year
                                                                                                ,"."
                                                                                                ,T.pack $ show month
                                                                                                ,"."
                                                                                                ,T.pack $ show day
                                                                                                ]))
                                                   dayLargeSplices place user (year, month, day)
                                                 _ -> do
                                                   today <- liftM utctDay $ liftIO getCurrentTime
                                                   return (today,[])
                               heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_large"
                                
monthH u p = do month <- fmap (maybeRead =<<) $ getParam "month"
                year <- fmap (maybeRead =<<) $ getParam "year"
                today <- liftM utctDay $ liftIO getCurrentTime
                let day = fromMaybe today $ liftM2 (\y m -> fromGregorian y m 1) year month
                setView u "work" (TE.encodeUtf8 (T.concat ["work.month" 
                                                          ,(maybe "" (T.append "." . T.pack . show ) year)
                                                          ,(maybe "" (T.append "." . T.pack . show) month)]))
                heistLocal (bindSplices ((monthSplices u p day Nothing) ++ (commonSplices day))) $ renderWS "work/month_calendar"

       
            
monthSplices u p curday day' = [("monthName", textSplice $ T.pack (formatTime defaultTimeLocale "%B %Y" curday))
                               ,("notLarge", identitySplice)
                               ,("large", blackHoleSplice)
                               ,("monthDays", monthView p u year month day')
                               ,("mNextYear",  textSplice $ T.pack $ show nextYear)
                               ,("mNextMonth", textSplice $ T.pack $ show nextMonth)
                               ,("mCurrYear",  textSplice $ T.pack $ show year)
                               ,("mCurrMonth", textSplice $ T.pack $ show month)
                               ,("mPrevYear",  textSplice $ T.pack $ show prevYear)
                               ,("mPrevMonth", textSplice $ T.pack $ show prevMonth)
                               ,("day", identitySplice) -- this is to allow the same template to be used in other ways
                               ]
  where (year,month,_) = toGregorian curday
        (nextYear,nextMonth,_) = toGregorian $ addGregorianMonthsClip 1 curday
        (prevYear,prevMonth,_) = toGregorian $ addGregorianMonthsClip (-1) curday
  
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

