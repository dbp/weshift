{-# LANGUAGE OverloadedStrings #-}

module Handlers.Timesheet where
  
import Snap.Types

import Text.Templating.Heist
import Snap.Extension.Heist
import qualified Text.XmlHtml as X

import Data.Maybe (fromJust, fromMaybe)

import Text.Digestive.Types
import Text.Digestive.Snap.Heist
import Text.Digestive.Validate
import Database.HDBC

import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Control.Applicative
import Control.Monad.Trans (liftIO, lift)

import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Calendar.OrdinalDate
import Data.Time.Format
import Data.Time.Clock
import System.Locale

import Data.List.Split
import Data.List (sortBy)

import Application
import Auth
import State.Types
import State.Shifts
import Common

getTimesheet :: UserPlace -> User -> Day -> Day -> Application [(Text, Splice Application)]
getTimesheet place user start stop = do
  allOriginal <- getOriginalShifts place start stop
  userCurrent <- getUserCurrentShifts place user start stop
  let userOriginal = filter ((==) (uId user) . sUser) allOriginal
  let othersOriginal = filter (\o -> sId o `elem` (map sId userCurrent) && sUser o /= uId user) allOriginal
  entries <- mapM timesheetEntry $ sortBy (\s1 s2 -> compare (sStart s1) (sStart s2)) $ othersOriginal ++ userOriginal
  return [ ("timesheet", mapSplices renderEntry entries)
         , ("timesheetStart", textSplice $ renderDate start)
         , ("timesheetStop", textSplice $ renderDate stop)
         ]

timesheetEntry shift = do
  tz <- liftIO getCurrentTimeZone
  let utcStart = localTimeToUTC tz $ sStart shift
  let utcEnd = localTimeToUTC tz $ sStop shift
  let hoursWorked = floor $ (diffUTCTime utcEnd utcStart) / (60*60)
  changes <- getShiftChanges shift
  deletes <- getShiftDeletes shift
  covers  <- getShiftCovers shift
  let modifications = sortBy (\m1 m2 -> compare (mTime m1) (mTime m2)) (changes ++ deletes ++ covers)
  return $ Entry hoursWorked (sStart shift) (sStop shift) modifications



data Entry = Entry Int LocalTime LocalTime [Modification] -- hours worked, orig. start, orig. end, list of modifications

renderTime t = T.pack $ formatTime defaultTimeLocale "%-l:%M%P" t
renderDate t = T.pack $ formatTime defaultTimeLocale "%m.%d.%Y" t
renderDateLong t = T.pack $ formatTime defaultTimeLocale "%B %e, %Y" t

renderChange (Delete u t) = runChildrenWithText [ ("changeClasses", "delete")
                                                , ("changeDescription", "Deleted")
                                                , ("changePerson", TE.decodeUtf8 $ uName u)
                                                , ("changeTime", renderTime t)
                                                , ("changeDate", renderDate t)
                                                ]


renderChange (Change s e u t) = runChildrenWithText [ ("changeClasses", "change")
                                                    , ("changeDescription", T.concat ["To ",(renderTime s),"-",(renderTime e)])
                                                    , ("changePerson", TE.decodeUtf8 $ uName u)
                                                    , ("changeTime", renderTime t)
                                                    , ("changeDate", renderDate t)
                                                    ]
                                                    

renderChange (Cover u t) = runChildrenWithText [ ("changeClasses", "cover")
                                               , ("changeDescription", "Covered")
                                               , ("changePerson", TE.decodeUtf8 $ uName u)
                                               , ("changeTime", renderTime t)
                                               , ("changeDate", renderDate t)
                                               ]
                                               
renderEntry (Entry hours start end changes) =
  runChildrenWith [ ("hoursWorked", textSplice $ T.pack $ show hours)
                  , ("startTime", textSplice $ renderTime start)
                  , ("endTime", textSplice $ renderTime end)
                  , ("shiftDate", textSplice $ renderDateLong start)
                  , ("changes", mapSplices renderChange changes)
                  ]
                  
