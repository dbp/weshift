{-# LANGUAGE OverloadedStrings #-}

module Time where 

import Control.Applicative hiding (optional)
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.LocalTime
import System.Locale (defaultTimeLocale)


import Data.Maybe (isJust, mapMaybe, catMaybes, fromMaybe, isNothing)
import Data.Char (digitToInt)
import Text.Parsec
import Text.Parsec.String

-- | This function tries to understand a date in as many ways as possible, stoping as soon as it finds one that works.
fuzzyParseDate :: String -> Maybe Day
fuzzyParseDate s = foldr ($) Nothing parseList
  where parseList = map (\format -> (\p -> if isJust p then p else parseTime defaultTimeLocale format s))
                      ["%-m/%-d/%Y"
                      ,"%-m/%-d/%y"
                      ,"%-m-%-d-%Y"
                      ,"%-m-%-d-%y"
                      ,"%-m.%-d.%y"
                      ,"%-m.%-d.%y"
                      ]
                      
data AP = AM | PM | NONE deriving Show

data TIME = KNOWN DiffTime | UNKNOWN DiffTime deriving Show

halfDay = 12*60*60
fullDay = 24*60*60

int = digitToInt <$> digit

parseHour = do d1 <- Just <$> int
               d2 <- optionMaybe int
               s <- optionMaybe (char ':')
               d3 <- optionMaybe int
               d4 <- optionMaybe int
               ampm <- choice [ string "am" >> return AM
                              , string "pm" >> return PM
                              , string "AM" >> return AM
                              , string "PM" >> return PM
                              , string "a.m." >> return AM
                              , string "p.m." >> return PM
                              , string "A.M." >> return AM
                              , return NONE
                              ]
               let prelimTime = 
                     case s of
                       Nothing -> -- in this case, we have to guess based on how many digits we have
                         let ds = catMaybes [d1,d2,d3,d4] in
                         case ds of
                           (x:[]) -> hours x
                           (x1:x2:[]) -> let n = (x1 * 10 + x2) in 
                                         if n > 24 {-assuming they forgot trailing 0-} 
                                           then minutes (x1 * 60 + x2 * 10) 
                                           else hours n 
                           _ -> let n = listToInt ds in minutes $ (n `div` 100) * 60 + (n `mod` 100)
                       _ -> -- this means there was a colon divider, so we can process before and after separately
                         let hs = listToInt $ catMaybes [d1,d2]
                             ms = listToInt $ catMaybes [d3,d4] in
                         minutes $ hs * 60 + ms
               case ampm of
                 AM -> if prelimTime < halfDay then return (KNOWN prelimTime) else fail "AM time above 12hrs"
                 PM -> if prelimTime < halfDay then return (KNOWN $ prelimTime + halfDay) else fail "PM time above 12hs"
                 NONE -> if prelimTime > fullDay then fail "time above 24hrs" else 
                         return $ if prelimTime > halfDay then KNOWN prelimTime {-this is 24hr time-} else UNKNOWN prelimTime
    where hours h = minutes (h * 60)
          minutes m = fromIntegral (m * 60) :: DiffTime
          listToInt ls = foldr (\(x,m) t -> t + (x * m)) 0 $ zip ls (reverse $ take (length ls) $ iterate (*10) 1)

timeRange :: Parser (DiffTime,DiffTime)
timeRange = do
    start <- parseHour
    spaces
    optional (char '-')
    spaces
    end <- parseHour
    -- now if the times are unknown, figure out our best guess of what they should be,
    -- based on our knowledge of the domain, ie, work shifts.
    case guessTime start end of
      Nothing -> fail "start time after end time"
      Just (s,e) -> return (s,e)

guessTime start end = 
  check $ case (start,end) of
             (KNOWN s, KNOWN e) -> 
               (s,e)
             (KNOWN s, UNKNOWN e') -> 
               if e' < s then (s,e' + halfDay) else (s,e')
             (UNKNOWN s', KNOWN e) -> 
               -- assume that a shift is less than 12hrs (otherwise, something evil is happening)
               if e - s' > halfDay then (s' + halfDay,e) else (s',e)
             (UNKNOWN s', UNKNOWN e') -> 
               -- here is where we make biggest guess - that shifts will not start before 5am, 
               -- and of course won't be more than 12hrs
               let s = if s' < 5*60*60 then s' + halfDay else s' in
               if e' < s then (s,e' + halfDay) else (s,e')
  where check (s,e) = if s < e then Just (s,e) else Nothing
