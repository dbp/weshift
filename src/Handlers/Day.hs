{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Handlers.Day where
  
import Snap.Types

import Text.Templating.Heist
import Snap.Extension.Heist
import qualified Text.XmlHtml as X

import Data.Maybe (fromJust, fromMaybe)
import Data.List (nub)

import Text.Digestive.Types
import Text.Digestive.Snap.Heist
import Text.Digestive.Validate
import Database.HDBC

import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Control.Applicative
import "mtl" Control.Monad.Trans (liftIO, lift)

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import System.Locale
import Data.Time.Calendar.OrdinalDate
import Data.List.Split

import Application
import Auth
import State.Types
import State.Shifts
import State.Coworkers
import Handlers.Shifts
import Common

daySplices :: User -> UserPlace -> [User] -> [Shift] -> Day -> [(T.Text, Splice Application)]
daySplices u p workers shifts day = 
  [("dayName", textSplice $ T.pack (formatTime defaultTimeLocale "%A, %B %-d" day))
  ,("dayWorkers", dayView u p workers shifts year month d)
  ,("timeColumn", timeColumn startTime dayLength)
  ,("columnHeight", textSplice $ T.pack $ show timeColumnHeight)
  ,("viewHeight", textSplice $ T.pack $ show $ timeColumnHeight + 5)
  ,("workersWidth", textSplice $ T.pack $ show $ 60 * (length workers))
  ,("dNextYear",  textSplice $ T.pack $ show nextYear)
  ,("dNextMonth", textSplice $ T.pack $ show nextMonth)
  ,("dNextDay", textSplice $ T.pack $ show nextDay)
  ,("dPrevYear",  textSplice $ T.pack $ show prevYear)
  ,("dPrevMonth", textSplice $ T.pack $ show prevMonth)
  ,("dPrevDay", textSplice $ T.pack $ show prevDay)
  ]
    where (year,month,d) = toGregorian day
          (nextYear,nextMonth,nextDay) = toGregorian $ addDays 1 day
          (prevYear,prevMonth,prevDay) = toGregorian $ addDays (-1) day
          startTime =  minimum $ (24*4) : (map (timePeriod . localTimeOfDay . sStart) shifts)
          endTime = maximum $ 0 : (map (timePeriod . localTimeOfDay . sStop) shifts)
          dayLength = (\n -> if n < 0 then 0 else n) $ endTime - startTime
          timeColumnHeight = dayLength + (4 - (dayLength `mod` 4)) + (4 - (startTime `mod` 4))
          
timeColumn start length = return $
                            (X.Element "div" [("class", T.append "top t-" (T.pack $ show $ 4 - (start `mod` 4)))] [])
                            :(map (\n -> X.Element "div" [("class","h-4 hour")] [X.TextNode (T.pack $ (show  (if n > 12 then n - 12 else n)) ++ ":00" ++ (if n > 12 then "pm" else "am"))]) 
                            $ take (max 2 ((length `div` 4)  + 1)) (iterate (+1) (start `div` 4 + (if start `mod` 4 == 0 then 0 else 1))))

dayView :: User -> UserPlace -> [User] -> [Shift] -> Integer -> Int -> Int -> Splice Application
dayView u p workers shifts year month day = do
  let start = fromGregorian year month day 
  let end = addDays 1 start
  uncoveredShifts <- lift $ getUncoveredShifts start end p
  let shiftUsers = map sUser shifts
  let dayLaborers = filter (\w -> (uId w) `elem` shiftUsers) workers
  let rest = filter (not . (flip elem dayLaborers)) workers
  let startTime = timePeriod $ localTimeOfDay $ minimum $ map sStart shifts
  renderDayVD workers u startTime shifts uncoveredShifts (dayLaborers ++ rest)

timePeriod tod = (todHour tod) * 4 + (todMin tod `div` 15)
  
renderDayVD workers self start s us ds = mapSplices (workerDay self start s us) ds

workerDay self start ss us dl = 
  runChildrenWith [("name", textSplice $ TE.decodeUtf8 $ uName dl)
                  ,("shifts", mapSplices sS $ reverse $ snd $ foldr buildSs (start,[]) (filter (\s -> sUser s == uId dl) ss))
                  ]        
    where buildSs s (offset,acc) = let start = timePeriod (localTimeOfDay (sStart s))
                                       offset' = start - offset
                                       stop = timePeriod (localTimeOfDay (sStop s))
                                       length = stop - start
                                       classes = if (uId self) == (sUser s) then "self" else "other"
                                       time = (formatTime defaultTimeLocale "%-I:%M%P" (sStart s)) ++ "-" ++ (formatTime defaultTimeLocale "%-I:%M%P" (sStop s)) in
                                       (stop, (offset',length,classes,time):acc)
          sS (offset,length,classes,time) = runChildrenWith [("offset", textSplice $ T.pack $ show offset)
                                                            ,("length", textSplice $ T.pack $ show length)
                                                            ,("classes", textSplice $ classes)
                                                            ,("time", textSplice $ T.pack time)
                                                            ]
    