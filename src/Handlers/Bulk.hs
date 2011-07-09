{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Handlers.Bulk where
  
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE  
import qualified Data.ByteString.Char8 as B8  
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar
import System.Locale (defaultTimeLocale)

import "mtl" Control.Monad.Trans
import Text.Templating.Heist
import Snap.Extension.Heist
import Text.SSV
import Data.Maybe (isJust, mapMaybe, catMaybes)
import Data.Char (digitToInt)
  
import Snap.Types
import Application
import State.Types
import Common
import Handlers.Shifts

import Text.Parsec
import Text.Parsec.String

bulkInputH :: User -> UserPlace -> Application ()
bulkInputH user place = 
  route [("/", ifTop $ method GET $ inputForm)
        ,("/upload", method POST $ upload user place)
        ]
        
inputForm = do
    today <- fmap utctDay $ liftIO getCurrentTime
    heistLocal (bindSplices (commonSplices today)) $ renderWS "work/bulk"
    
upload u p = do
  md <- getParam "data"
  case md of
    Nothing -> redirPlaceHomeAsync
    Just d -> do
      let (understood, notUnderstood) = parseShifts $ transformCSV $ readCSV (B8.unpack d)
      heistLocal (bindSplices [("understood", renderShifts understood)
                              ,("not-understood", renderNotUnderstood notUnderstood)]) 
                $ renderWS "work/bulk_understood"

renderNotUnderstood :: Monad m => [(String,Day,String)] -> Splice m
renderNotUnderstood = mapSplices (\(name, date, shift) -> 
                           runChildrenWith [("name", textSplice $ T.pack name)
                                           ,("date", textSplice $ T.pack $ show date)
                                           ,("shift", textSplice $ T.pack shift)
                                           ])

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

-- | transformCSV makes it so that each cell is a standalone three-tuple that is the name, date, and shift
transformCSV :: [[String]] -> [(String,Day,String)]
transformCSV [] = []
transformCSV csv@(_:_:_) | length headings >= maximum (map length fields) = concatMap transformFields fields
  where headings = (fromGregorian 0 0 0)  : (mapMaybe fuzzyParseDate $ head csv)
        fields = tail csv
        transformFields [] = []
        transformFields (name:shifts) = map (\(s,index) -> (name,headings !! (index + 1), s)) $ zip shifts (iterate (+1) 0)
transformCSV _ = []
           
parseShifts :: [(String,Day,String)] -> ([Shift],[(String,Day,String)])
parseShifts fields = ([],fields)
  {-where ps = map (\(person,day,timerange) -> ) fields-}

data AP = AM | PM | NONE deriving Show

data TIME = KNOWN NominalDiffTime | UNKNOWN NominalDiffTime deriving Show

time = do d1 <- fmap (Just . digitToInt) digit
          d2 <- option Nothing (fmap (Just . digitToInt) digit)
          s <- option Nothing (fmap Just $ char ':')
          d3 <- option Nothing (fmap (Just . digitToInt) digit)
          d4 <- option Nothing (fmap (Just . digitToInt) digit)
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
                      (x1:x2:[]) -> hours (x1 * 10 + x2)
                      _ -> let n = listToInt ds in minutes $ (n `mod` 100) * 60 + (n `div` 100)
                  _ -> -- this means there was a divider, so we can process before and after separately
                    let hs = listToInt $ catMaybes [d1,d2]
                        ms = listToInt $ catMaybes [d3,d4] in
                    minutes $ hs * 60 + ms
          case ampm of
            AM -> return $ KNOWN prelimTime
            PM -> return $ KNOWN $ prelimTime + 12*60*60
            NONE -> return $ if prelimTime > 12*60*60 then KNOWN prelimTime {-this is 24hr time-} else UNKNOWN prelimTime
    where hours h = minutes (h * 60)
          minutes m = fromInteger $ toInteger (m * 60) :: NominalDiffTime
          listToInt ls = foldr (\(x,m) t -> t + (x * m)) 0 $ zip ls (reverse $ take (length ls) $ iterate (*10) 1)

parseTimeRange :: Parser (NominalDiffTime,NominalDiffTime)
parseTimeRange = do
  start <- time
  spaces
  optional (char '-')
  spaces
  end <- time
  -- now if the times are unknown, figure out our best guess of what they should be,
  -- based on our knowledge of the domain, ie, work shifts.
  return $ case (start,end) of
            (KNOWN s, KNOWN e) -> 
              (s,e)
            (KNOWN s, UNKNOWN e') -> 
              if e' - s < 0 then (s,e' + 12*60*60) else (s,e')
            (UNKNOWN s', KNOWN e) -> 
              -- assume that a shift is less than 12hrs (otherwise, something evil is happening)
              if e - s' > 12*60*60 then (s' + 12*60*60,e) else (s',e)
            (UNKNOWN s', UNKNOWN e') -> 
              -- here is where we make biggest guess - that shifts will not start before 5am, 
              -- and of course won't be more than 12hrs
              let s = if s' < 5*60*60 then s' + 12*60*60 else s' in
              if e' - s < 0 then (s,e' + 12*60*60) else (s,e')



