{-# LANGUAGE OverloadedStrings #-}

module Handlers.Month where
  
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
import Data.List.Split

import Application
import Auth
import State.Types
import State.Shifts
import Common

data DayFormat = DayFormat { dNumber :: Maybe Int -- what day of the month is it, if any
                           , dMyShift :: Bool -- do I have shifts this day
                           , dShifts :: Bool -- does anyone have a shift this day
                           , dTop :: Bool -- is this at the top of the calendar
                           , dStart :: Bool -- is this the start of a week
                           , dEnd :: Bool -- is this the end of a week
                           , dBottom :: Bool -- is this the bottom of the calendar
                           , dRequest :: Bool -- are there requets this day
                           }

emptyDayFormat n = DayFormat n False False False False False False False
                           
renderDay :: DayFormat -> Splice Application
renderDay (DayFormat num myshifts shifts top start end bottom request) =
  runChildrenWithText [ ("boxClasses", boxclasses)
                      , ("dayClasses", dayclasses)
                      , ("dayNum", (maybe " " (T.pack . show) num))
                      ]
  {-  return [X.Element "div" [("class", boxclasses)] 
             [X.Element "div" [("class", dayclasses)] 
               [X.TextNode (maybe " " (T.pack . show) num)]]]-}
      where boxclasses = T.concat (["daybox"] ++ 
                                  (if start then [" start"] else []) ++ 
                                  (if end then [" end"] else []) ++ 
                                  (if top then [" top"] else []) ++
                                  (if bottom then [" bottom"] else []) ++
                                  (if request then [" request"] else []))
            dayclasses = T.concat (["day"] ++
                                  (if myshifts then [" self"] else if shifts then [" other"] else []) ++
                                  maybe [" none"] (const []) num)
                       
renderMonth :: [DayFormat] -> Splice Application
renderMonth days = mapSplices renderDay days

buildMonth :: Integer -> Int -> [[DayFormat]]
buildMonth year month = splitEvery 7 $ map emptyDayFormat $ 
                                        (take offset $ repeat Nothing) ++ 
                                        (map Just $ take monthLength (iterate (+1) 1)) ++
                                        (take lastoffset $ repeat Nothing) 
  where start = fromGregorian year month 1
        offset = snd $ sundayStartWeek start
        monthLength = gregorianMonthLength year month
        lastoffset = 6 - (snd $ sundayStartWeek $ addDays (toInteger $ monthLength - 1) start)

top df = df { dTop = True }
start df = df { dStart = True }
end df = df { dEnd = True }
bottom df = df { dBottom = True }
myshift df = df { dMyShift = True }
othershift df = df { dShifts = True }
request df = df { dRequest = True }

formatDay :: Integer -> Int -> [Shift] -> [Shift] -> User -> DayFormat -> DayFormat 
formatDay y m shifts uncovered user day = (if myShiftsToday then myshift else if otherShiftToday then othershift else id) $
                                                 (if requestsToday then request else id) $
                                                 day
    where currDay = addDays (toInteger $ fromMaybe 1 (dNumber day) - 1) (fromGregorian y m 1)
          shiftsToday = filter (\s -> (localDay $ sStart s) == currDay) shifts
          myShiftsToday = elem (uId user) $ map sUser $ shiftsToday
          otherShiftToday = not $ null shiftsToday
          requestsToday = not $ null $ filter (\s -> (localDay $ sStart s) == currDay) uncovered

formatWeek :: Integer -> Int -> [Shift] -> [Shift] -> User -> [DayFormat]  -> [DayFormat]
formatWeek y m s c u week = map (formatDay y m s c u) $ 
                              (start (head week)) : ((init $ tail week) ++ [end (last week)])

formatMonth :: Integer -> Int -> [Shift] -> [Shift] -> User -> [[DayFormat]] -> [[DayFormat]]
formatMonth y m s c u weeks = map (formatWeek y m s c u) $ 
                                (map top (head weeks)) : ((init $ tail weeks) ++ [map bottom (last weeks)])

monthView :: Integer -> Int -> Splice Application
monthView year month = do let start = fromGregorian year month 1
                          mplace <- lift getCurrentPlace
                          muser <- lift getCurrentUser
                          case (mplace, muser) of
                            (Just place, Just user) -> do
                              let end = addDays (toInteger $ gregorianMonthLength year month) start
                              shifts <- lift $ getShifts start end place
                              uncoveredShifts <- lift $ getUncoveredShifts start end place
                              renderMonth $ concat $ formatMonth year month shifts uncoveredShifts user $ buildMonth year month
                            _ -> return []
                            
