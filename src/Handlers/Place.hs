{-# LANGUAGE OverloadedStrings #-}

module Handlers.Place where

  -- | Boilerplate imports
import qualified Data.Bson             as B
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map              as M
import           Data.Monoid
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Imports
import qualified Text.XmlHtml          as X
import qualified Utils                 as U

-- | Module specific imports
import           State.Account
import           State.Coworkers
import           State.Shifts

import           Handlers.Bulk
import           Handlers.Coworkers
import           Handlers.Help
import           Handlers.Messages
import           Handlers.Messages
import           Handlers.Settings
import           Handlers.Shifts
import           Handlers.Timesheet

import           Render.Calendar
import           Render.Coworkers
import           Render.Shifts
import           Render.Timesheet

placeSite :: AppHandler ()
placeSite = do
  orgName <- fmap TE.decodeUtf8 <$>  getParam "organization"
  placeName <- fmap TE.decodeUtf8 <$> getParam "place"
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
            , ("/review",                        reviewH)
            , ("/coworkers",                     coworkersH)
            , ("/help",                          helpH)
            , ("/settings",                      settingsH)
            , ("/shift",                         shiftH)
            , ("/messages",                      messagesH)
            , ("/blank",                         (\_ _ -> renderWS "profile/blank"))])

placeHomeH u p = do today <- liftM utctDay $ liftIO getCurrentTime
                    nextShift <- getNextShift u p
                    nextDeadline <- getNextDeadline u p
                    let nextShiftSplice = spliceMBS "nextShift" $ Just $ fromMaybe "No Next Shift" $ liftM (T.pack . (formatTime defaultTimeLocale "%-l:%M%P, %-e %-B  %Y").sStart) nextShift
                    let nextDeadlineSplice = spliceMBS "nextDeadline" $ Just $ fromMaybe "No Next Deadline" $ liftM (T.pack . (formatTime defaultTimeLocale "%-l:%M%P, %-e %-B  %Y").sStart) nextDeadline
                    workers <- getWorkers p
                    let coworkers = filter ((/= (uId u)) . uId) workers
                    let monthday = maybe today (\(y,m,d') -> fromGregorian y m (fromMaybe 1 d')) savedMonth
                    let dayday = maybe today (\(y,m,d) -> fromGregorian y m d) savedDay
                    shifts <- getShifts dayday (addDays 1 dayday) p
                    timesheetSplice <- getTimesheet p u (addDays (-14) today) today
                    reviewSplice <- if pFac p then getReview p (addDays (-14) today) today else return mempty
                    emails <- getUserEmails u
                    let showDay = case savedMonth of
                                    Just (_,_,Just d) -> Just d
                                    _ -> Nothing
                    let emailsSplice = "emails" ## renderEmails emails
                    messagesSplice <- messagesPageSplices p 1
                    heistLocal (bindSplices (nextShiftSplice <> nextDeadlineSplice <> (commonSplices today) <> (monthSplices u p monthday showDay) <>
                                             (coworkersSplice coworkers) <> (daySplices u p workers shifts dayday) <>
                                             timesheetSplice <> ("timesheetCoworkers" ## renderTSCoworkers u coworkers) <> reviewSplice <>
                                             emailsSplice <> messagesSplice)) $ renderWS "place"
      where mList Nothing = []
            mList (Just xs) = xs
            monthV = fmap (T.splitOn ".") $ getView u "work.month."
            savedMonth =
              case mList monthV of
                (y:m:md) -> do month <- maybeRead m
                               year <- maybeRead y
                               let d = case md of
                                        (d':[]) -> maybeRead d'
                                        _ -> Nothing
                               return (year,month,d)
                _ -> Nothing
            dayV = fmap (T.splitOn ".") $ getView u "work.day."
            savedDay =
              case mList dayV of
                (y:m:d:[]) -> do year <- maybeRead y
                                 month <- maybeRead m
                                 day <- maybeRead d
                                 return (year,month,day)
                _ -> Nothing

monthDaySmallH user place = do mmonth <- fmap TE.decodeUtf8 <$> getParam "month"
                               myear  <- fmap TE.decodeUtf8 <$>  getParam "year"
                               mday   <- fmap TE.decodeUtf8 <$>  getParam "day"
                               (day, daySplice) <-
                                  case (mmonth >>= maybeRead, myear >>= maybeRead, mday >>= maybeRead) of
                                    (Just month, Just year, Just day) -> do
                                      let d = (fromGregorian year month day)
                                      shifts <- getShifts d (addDays 1 d) place
                                      setView user "work" (T.concat ["work.month."
                                                                    ,T.pack $ show year
                                                                    ,"."
                                                                    ,T.pack $ show month
                                                                    ])

                                      return (d,("day" ## renderDay user Nothing [] $ formatDay year month shifts [] user (emptyDayFormat (Just day))))
                                    _ -> do
                                      today <- liftM utctDay $ liftIO getCurrentTime
                                      return (today, mempty)
                               heistLocal (bindSplices (daySplice <> (commonSplices day))) $ renderWS "work/month_day_small"

monthDayLargeH user place = do mmonth <- fmap TE.decodeUtf8 <$>  getParam "month"
                               myear  <- fmap TE.decodeUtf8 <$>  getParam "year"
                               mday   <- fmap TE.decodeUtf8 <$>  getParam "day"
                               (day,daySplice) <-
                                    case (mmonth >>= maybeRead, myear >>= maybeRead, mday >>= maybeRead) of
                                                 (Just month, Just year, Just day) -> do
                                                   setView user "work" (T.concat ["work.month"
                                                                                 ,"."
                                                                                 ,T.pack $ show year
                                                                                 ,"."
                                                                                 ,T.pack $ show month
                                                                                 ,"."
                                                                                 ,T.pack $ show day
                                                                                 ])
                                                   dayLargeSplices place user (year, month, day)
                                                 _ -> do
                                                   today <- liftM utctDay $ liftIO getCurrentTime
                                                   return (today, mempty)
                               heistLocal (bindSplices (daySplice <> (commonSplices day))) $ renderWS "work/month_day_large"

monthH u p = do month <- fmap (maybeRead . TE.decodeUtf8 =<<) $ getParam "month"
                year <- fmap (maybeRead . TE.decodeUtf8 =<<) $ getParam "year"
                today <- liftM utctDay $ liftIO getCurrentTime
                let day = fromMaybe today $ liftM2 (\y m -> fromGregorian y m 1) year month
                setView u "work" (T.concat ["work.month"
                                           ,(maybe "" (T.append "." . T.pack . show ) year)
                                           ,(maybe "" (T.append "." . T.pack . show) month)])
                heistLocal (bindSplices ((monthSplices u p day Nothing) <> (commonSplices day))) $ renderWS "work/month_calendar"



dayH u p = do
  day <- fmap (maybeRead . TE.decodeUtf8 =<<) $ getParam "day"
  month <- fmap (maybeRead . TE.decodeUtf8 =<<) $ getParam "month"
  year <- fmap (maybeRead . TE.decodeUtf8 =<<) $ getParam "year"
  today <- liftM utctDay $ liftIO getCurrentTime
  let curday = fromMaybe today $ liftM3 fromGregorian year month day
  setView u "work" (T.concat ["work.day"
                             ,(maybe "" (T.append "." . T.pack . show) year)
                             ,(maybe "" (T.append "." . T.pack . show) month)
                             ,(maybe "" (T.append "." . T.pack . show) day)
                             ])
  shifts <- getShifts curday (addDays 1 curday) p
  workers <- getWorkers p
  heistLocal (bindSplices ((commonSplices curday) <>
                           (daySplices u p workers shifts curday))) $ renderWS "work/day_calendar"


reviewH user place = case pFac place of
  False -> mzero
  True -> do
    mstart <- fmap TE.decodeUtf8 <$> getParam "start"
    mstop <- fmap TE.decodeUtf8 <$>  getParam "stop"

    --workers <- getWorkers place
    --let workersSplice = [("reviewCoworkers", renderTSCoworkers user' coworkers)]
    today <- liftM utctDay $ liftIO getCurrentTime

    reviewSplice <- case (mstart >>= parseWSDate,mstop >>= parseWSDate) of
          (Just start, Just stop) -> getReview place start stop
          _ -> getReview place (addDays (-14) today) today

    setView user "work" "work.review"

    heistLocal (bindSplices (commonSplices today <> reviewSplice)) $ renderWS "work/review"

getReview place start stop = do
  claims <- getUnresolvedClaims place start stop
  shifts <- getUserModifiedShifts place start stop
  return $ do ("unresolvedClaims" ## mapSplices (\(shift, (Claim id' sid user units reason resolved accepted)) ->
                                runChildrenWith $ do "id" ## textSplice id'
                                                     "shift" ## renderShift shift
                                                     "user" ## textSplice user
                                                     "units" ## textSplice $ T.pack $ show units
                                                     "reason" ## textSplice reason
                                                     "resolved" ## booleanSplice resolved
                                                     "notResolved" ## booleanSplice $ not resolved
                                                     "accepted" ## booleanSplice accepted
                                                     "notAccepted" ## booleanSplice $ not accepted
                                                     )
                                claims)
              "userModifiedShifts" ## mapSplices (\(shift, mods) ->
                                    runChildrenWith $ do "shift" ## renderShift shift
                                                         "changes" ## mapSplices (renderChange (sDeadline shift)) mods
                                                         )
                                    shifts
              "reviewStart" ## textSplice $ renderDate start
              "reviewStop" ## textSplice $ renderDate stop
