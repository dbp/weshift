{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Handlers.Timesheet where


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
import State.Account
import State.Coworkers
import Render.Timesheet



timesheetH user place = do
  targetUser <- getParam "user"
  mstart <- getParam "start"
  mstop <- getParam "stop"
  
  user' <- fmap fromJust $ case targetUser of
            Just tU -> if pFac place then getUser tU else return $ Just user
            Nothing -> return $ Just user
            
  coworkers <- getCoworkers user' place
  let coworkersSplice = [("timesheetCoworkers", renderTSCoworkers user' coworkers)]
  
  today <- liftM utctDay $ liftIO getCurrentTime
  
  timesheetSplice <- case (mstart >>= parseWSDate,mstop >>= parseWSDate) of
                        (Just start, Just stop) -> getTimesheet place user' start stop
                        _ -> getTimesheet place user' (addDays (-14) today) today
  
  setView user "work" "work.timesheet"

  heistLocal (bindSplices (commonSplices today ++ coworkersSplice ++ timesheetSplice)) $ renderWS "work/timesheet"


getTimesheet :: UserPlace -> User -> Day -> Day -> AppHandler [(Text, Splice AppHandler)]
getTimesheet place user start stop = do
  allOriginal <- getOriginalShifts place start (addDays 1 stop) -- because they expect it to be inclusive of stop day
  userCurrent <- getUserCurrentShifts place user start (addDays 1 stop)
  let userOriginal = filter ((==) (uId user) . sUser) allOriginal
  let othersOriginal = filter (\o -> sId o `elem` (map sId userCurrent) && sUser o /= uId user) allOriginal
  entries <- mapM (timesheetEntry user) $ sortBy (\s1 s2 -> compare (sStart s1) (sStart s2)) $ othersOriginal ++ userOriginal
  return [ ("timesheet", mapSplices renderEntry entries)
         , ("timesheetStart", textSplice $ renderDate start)
         , ("timesheetStop", textSplice $ renderDate stop)
         , ("totalHours", textSplice $ T.pack $ show $ sum $ map entryHours entries)
         , ("totalUnits", textSplice $ T.pack $ show $ sum $ map entryUnits entries)
         ]

timesheetEntry user shift = do
  tz <- liftIO getCurrentTimeZone
  let utcStart = localTimeToUTC tz $ sStart shift
  let utcEnd = localTimeToUTC tz $ sStop shift
  let units = sUnits shift
  changes <- getShiftChanges shift
  deletes <- getShiftDeletes shift
  covers  <- getShiftCovers shift
  let modifications = sortBy (\m1 m2 -> compare (mTime m1) (mTime m2)) (changes ++ deletes ++ covers)
  let (hoursWorked,unitsWorked) = getHours user tz utcStart utcEnd units modifications
  return $ Entry hoursWorked unitsWorked (sStart shift) (sStop shift) modifications


getHours user tz defstart defstop units [] = 
  (roundHours $ (diffUTCTime defstop defstart) / (60*60), units)
getHours user tz defstart defstop units modifications = 
    case last modifications of
      Cover coverer _ -> 
        if (uId user) == (uId coverer)
        then (getHours user tz defstart defstop units (tail modifications))
        else (0, 0)
      Delete _ _ -> (0, 0)
      Change ns ne _ u _ _ -> 
        (roundHours $ (diffUTCTime (localTimeToUTC tz ne) (localTimeToUTC tz ns)) / (60*60), u)

roundHours n = (/ 10) $ fromIntegral $ floor $ n * 10 