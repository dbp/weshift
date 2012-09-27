{-# LANGUAGE OverloadedStrings #-}

module Render.Calendar where


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

-- | Module Specific imports
import State.Shifts
import Forms.Shifts
import Render.Shifts


data DayFormat = DayFormat { dNumber :: Maybe Int -- what day of the month is it, if any
                           , dMyShift :: Bool -- do I have shifts this day
                           , dShifts :: Bool -- does anyone have a shift this day
                           , dTop :: Bool -- is this at the top of the calendar
                           , dStart :: Bool -- is this the start of a week
                           , dEnd :: Bool -- is this the end of a week
                           , dBottom :: Bool -- is this the bottom of the calendar
                           , dRequest :: Bool -- are there requests this day
                           }

emptyDayFormat n = DayFormat n False False False False False False False
                    
                       
renderMonth :: User -> Maybe Int -> [Shift] -> [DayFormat] -> Splice AppHandler
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

monthView :: UserPlace -> User -> Integer -> Int -> Maybe Int -> Splice AppHandler
monthView place user year month day' = do 
  let start = fromGregorian year month 1
  let end = addDays (toInteger $ gregorianMonthLength year month) start
  shifts' <- lift $ getShifts start end place
  let shifts = filter (\s -> if sDeadline s then not (sDeadlineDone s) else True) shifts'
  uncoveredShifts <- lift $ getUncoveredShifts start end place
  renderMonth user day' shifts $ concat $ formatMonth year month shifts uncoveredShifts user $ buildMonth year month


            
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

-- | the first three params are just for the case that a large day should alse be rendered within.         
renderDay :: User -> Maybe Int -> [Shift] -> DayFormat -> Splice AppHandler
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
                          


daySplices :: User -> UserPlace -> [User] -> [Shift] -> Day -> [(T.Text, Splice AppHandler)]
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

dayView :: User -> UserPlace -> [User] -> [Shift] -> Integer -> Int -> Int -> Splice AppHandler
dayView u p workers shifts' year month day = do
  -- don't include deadlines - they don't really make sense on a day calendar, as
  -- they can overlap with stuff.
  let shifts = filter (not . sDeadline) shifts'
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
                                       len = stop - start
                                       classes = T.concat [if (uId self) == (sUser s) then "self" else "other",
                                                           " ", T.pack (show (sColor s))]
                                       time = (formatTime defaultTimeLocale "%-I:%M%P" (sStart s)) ++ "-" ++ (formatTime defaultTimeLocale "%-I:%M%P" (sStop s)) in
                                       (stop, (offset',len,classes,time):acc)
          sS (offset,len,classes,time) = runChildrenWith [("offset", textSplice $ T.pack $ show offset)
                                                         ,("length", textSplice $ T.pack $ show len)
                                                         ,("classes", textSplice $ classes)
                                                         ,("time", textSplice $ T.pack time)
                                                         ]
    