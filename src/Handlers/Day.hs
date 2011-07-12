{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Handlers.Day where
  
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

daySplices :: User -> UserPlace -> [Shift] -> Day -> [(T.Text, Splice Application)]
daySplices u p shifts day = 
  [("dayName", textSplice $ T.pack (formatTime defaultTimeLocale "%A, %B %-d" day))
  ,("dayWorkers", dayView u p shifts year month d)
  ,("timeColumn", timeColumn startTime dayLength)
  ,("columnHeight", textSplice $ T.pack $ show dayLength)
  ,("nextYear",  textSplice $ T.pack $ show nextYear)
  ,("nextMonth", textSplice $ T.pack $ show nextMonth)
  ,("nextDay", textSplice $ T.pack $ show nextDay)
  ,("prevYear",  textSplice $ T.pack $ show prevYear)
  ,("prevMonth", textSplice $ T.pack $ show prevMonth)
  ,("prevDay", textSplice $ T.pack $ show prevDay)
  ]
    where (year,month,d) = toGregorian day
          (nextYear,nextMonth,nextDay) = toGregorian $ addDays 1 day
          (prevYear,prevMonth,prevDay) = toGregorian $ addDays (-1) day
          startTime =  minimum $ (24*4) : (map (timePeriod . localTimeOfDay . sStart) shifts)
          endTime = maximum $ 0 : (map (timePeriod . localTimeOfDay . sStop) shifts)
          dayLength = endTime - startTime
          
timeColumn start length = return $
                            (X.Element "div" [("class", T.append "h-" (T.pack $ show $ start `mod` 4))] [])
                            :(map (\n -> X.Element "div" [("class","h-4")] [X.TextNode (T.pack $ show  n)]) 
                            $ take (length `div` 4) (iterate (+1) (start `div` 4)))

dayView :: User -> UserPlace -> [Shift] -> Integer -> Int -> Int -> Splice Application
dayView u p shifts year month day = do
  let start = fromGregorian year month day 
  let end = addDays 1 start
  uncoveredShifts <- lift $ getUncoveredShifts start end p
  workers <- lift $ getWorkers p
  let shiftUsers = map sUser shifts
  let dayLaborers = filter (\w -> (uId w) `elem` shiftUsers) workers
  let startTime = timePeriod $ localTimeOfDay $ minimum $ map sStart shifts
  renderDayVD startTime shifts uncoveredShifts dayLaborers

timePeriod tod = (todHour tod) * 4 + (todMin tod `div` 15)
  
renderDayVD start s us ds = mapSplices (workerDay start s us) ds

workerDay start ss us dl = 
  runChildrenWith [("name", textSplice $ TE.decodeUtf8 $ uName dl)
                  ,("shifts", mapSplices sS $ reverse $ snd $ foldr buildSs (start,[]) (filter (\s -> sUser s == uId dl) ss))
                  ]        
    where buildSs s (offset,acc) = let start = timePeriod (localTimeOfDay (sStart s))
                                       offset' = start - offset
                                       stop = timePeriod (localTimeOfDay (sStop s))
                                       length = stop - start in
                                       (stop, (offset',length):acc)
          sS (offset,length) = runChildrenWith [("offset", textSplice $ T.pack $ show offset)
                                               ,("length", textSplice $ T.pack $ show length)
                                               ]
    