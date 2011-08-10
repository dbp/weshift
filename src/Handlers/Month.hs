{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Handlers.Month where
  
import Snap.Types

import Text.Templating.Heist
import Snap.Extension.Heist
import qualified Text.XmlHtml as X

import Data.Maybe (fromJust, fromMaybe, isJust)

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

-- | the first three params are just for the case that a large day should alse be rendered within.         
renderDay :: User -> Maybe Int -> [Shift] -> DayFormat -> Splice Application
renderDay u day' shifts (DayFormat num myshifts othershifts top start end bottom request) =
  runChildrenWith $ [ ("boxClasses", textSplice boxclasses)
                    , ("dayClasses", textSplice dayclasses)
                    , ("dayNum", textSplice (maybe " " (T.pack . show) num))
                    ] ++ extra
      where boxclasses = T.concat (["daybox"] ++ 
                                  (if start then [" start"] else []) ++ 
                                  (if end then [" end"] else []) ++ 
                                  (if top then [" top"] else []) ++
                                  (if bottom then [" bottom"] else []) ++
                                  (if request then [" request"] else []))
            dayclasses = T.concat (["day"] ++
                                  (if myshifts then [" self"] else if othershifts then [" other"] else []) ++
                                  maybe [" none"] (const []) num)
            -- this means that we are also showing the big day.
            selfShifts = filter (\s -> ((uId u) == (sUser s)) && onToday s) shifts
            otherShifts = filter (\s -> ((uId u) /= (sUser s)) && onToday s) shifts
            onToday s = Just (trd (toGregorian (localDay (sStart s)))) == day'
            trd (_,_,a) = a 
            extra = case day' of
                      Just d | Just d == num ->
                        [("notLarge", blackHoleSplice)
                        ,("large", identitySplice)
                        ,("start-value", textSplice "9:00am")
                        ,("stop-value", textSplice "5:00pm")
                        ,("start-errors", blackHoleSplice)
                        ,("stop-errors", blackHoleSplice)
                        ,("id-errors", blackHoleSplice)
                        ,("selfShifts",  renderShifts selfShifts)
                        ,("otherShifts", renderShifts otherShifts)
                        ,("selfClasses", textSplice $ if null selfShifts then "" else "shift")
                        ]
                      _ -> []
                    
                       
renderMonth :: User -> Maybe Int -> [Shift] -> [DayFormat] -> Splice Application
renderMonth u d' ss days = mapSplices (renderDay u d' ss) days

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
formatDay y m shifts uncovered user day | isJust (dNumber day) = 
        (if myShiftsToday then myshift else if otherShiftToday then othershift else id) $
        (if requestsToday then request else id) $
        day
    where currDay = addDays (toInteger $ fromMaybe 1 (dNumber day) - 1) (fromGregorian y m 1)
          shiftsToday = filter (\s -> (localDay $ sStart s) == currDay) shifts
          myShiftsToday = elem (uId user) $ map sUser $ shiftsToday
          otherShiftToday = not $ null shiftsToday
          requestsToday = not $ null $ filter (\s -> (localDay $ sStart s) == currDay) uncovered
formatDay _ _ _ _ _ day = day

formatWeek :: Integer -> Int -> [Shift] -> [Shift] -> User -> [DayFormat]  -> [DayFormat]
formatWeek y m s c u week = map (formatDay y m s c u) $ 
                              (start (head week)) : ((init $ tail week) ++ [end (last week)])

formatMonth :: Integer -> Int -> [Shift] -> [Shift] -> User -> [[DayFormat]] -> [[DayFormat]]
formatMonth y m s c u weeks = map (formatWeek y m s c u) $ 
                                (map top (head weeks)) : ((init $ tail weeks) ++ [map bottom (last weeks)])

monthView :: UserPlace -> User -> Integer -> Int -> Maybe Int -> Splice Application
monthView place user year month day' = do 
  let start = fromGregorian year month 1
  let end = addDays (toInteger $ gregorianMonthLength year month) start
  shifts <- lift $ getShifts start end place
  uncoveredShifts <- lift $ getUncoveredShifts start end place
  renderMonth user day' shifts $ concat $ formatMonth year month shifts uncoveredShifts user $ buildMonth year month
 
 
dayLargeSplices place user (year, month, day) = do
   let d = (fromGregorian year month day)
   shifts <- getShifts d (addDays 1 d) place
   let selfShifts = filter ((== (uId user)) . sUser) shifts
   let otherShifts = filter ((/= (uId user)) . sUser) shifts
   let selfClasses = if null selfShifts then "" else "shift"
   return (d,[("day", renderDay user Nothing [] $ formatDay year month shifts [] user (emptyDayFormat (Just day)))
             ,("closeDays", mapSplices (\n -> runChildrenWithText [("dayNum",T.pack $ show n)]) (filter (/= day) $ take (gregorianMonthLength year month) $ iterate (+1) 1))
             ,("start-value", textSplice "9:00am")
             ,("stop-value", textSplice "5:00pm")
             ,("start-errors", blackHoleSplice)
             ,("stop-errors", blackHoleSplice)
             ,("id-errors", blackHoleSplice)
             ,("selfShifts", renderShifts selfShifts)
             ,("otherShifts", renderShifts otherShifts)
             ,("selfClasses", textSplice $ selfClasses)
             ])

renderShift :: Shift -> Splice Application
renderShift (Shift id' user place start stop recorded recorder) = do
  req <- lift $ maybe (return Nothing) (getShiftRequest user) (if id' == "" then Nothing else Just id')
  runChildrenWith [("id", textSplice $ TE.decodeUtf8 id')
                  ,("user", textSplice $ TE.decodeUtf8 user)
                  ,("place", textSplice $ TE.decodeUtf8 place)
                  ,("date", textSplice $ renderDate start)
                  ,("start", textSplice $ renderTime start)
                  ,("stop", textSplice $ renderTime stop)
                  ,("recorded", textSplice $ renderTime recorded)
                  ,("recorder", textSplice $ TE.decodeUtf8 recorder)
                  ,("ifRequested", if isJust req then identitySplice else blackHoleSplice)
                  ,("notRequested", if isJust req then blackHoleSplice else identitySplice)
                  ,("reqid", textSplice $ TE.decodeUtf8 $ fromMaybe "" req)
                  ]

renderShifts :: [Shift] -> Splice Application
renderShifts ss = mapSplices renderShift ss                             
