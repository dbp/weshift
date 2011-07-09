{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Handlers.Bulk where
  
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE  
import qualified Data.ByteString.Char8 as B8  
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.LocalTime
import System.Locale (defaultTimeLocale)

import "mtl" Control.Monad.Trans
import Text.Templating.Heist
import Snap.Extension.Heist
import Text.SSV
import Data.Maybe (isJust, mapMaybe, catMaybes, fromMaybe, isNothing)
import Data.Char (digitToInt)
import Data.List (find,nub)
import Data.Either (lefts,rights)
  
import Snap.Types
import Application
import State.Types
import State.Coworkers
import State.Shifts
import Common
import Handlers.Shifts

import Text.Parsec
import Text.Parsec.String

bulkInputH :: User -> UserPlace -> Application ()
bulkInputH user place = 
  route [("/", ifTop $ method GET $ inputForm)
        ,("/upload", method POST $ upload user place)
        ,("/confirm", method POST $ confirm user place)
        ]
        
inputForm = do
    today <- fmap utctDay $ liftIO getCurrentTime
    heistLocal (bindSplices (commonSplices today)) $ renderWS "work/bulk"
    
upload u p = do
  md <- getParam "data"
  case md of
    Nothing -> redirPlaceHomeAsync
    Just d -> do
      workers <- if pFac p then getWorkers p else return [u] -- if they are not a facilitator, they can only load their own shifts
      let (understood, notUnderstood) = parseShifts workers $ transformCSV $ readCSV (B8.unpack d)
      heistLocal (bindSplices [("understood", renderShifts understood)
                              ,("understood-serialized", textSplice $ TE.decodeUtf8 $ urlEncode $ B8.pack $ show understood)
                              ,("not-understood", renderNotUnderstood notUnderstood)
                              ,("not-understood-serialized", textSplice $ TE.decodeUtf8 $ urlEncode $ B8.pack $ show notUnderstood)
                              ]) 
                $ renderWS "work/bulk_understood"

confirm u p = do
  mshifts <- getParam "understood"
  n <- getParam "notunderstood"
  case (mshifts >>= urlDecode >>= maybeRead, n >>= urlDecode >>= maybeRead) of
    (Just shifts, Just notUnderstood) -> do
      workers <- getWorkers p
      res <- mapM insertShift $ filterFac $ map (\s -> s { sPlace = (pId p), sRecorder = (uId u) }) shifts
      let numSuccess = length $ filter isNothing res
      let numFailed = length $ filter isJust res
      let unparsed = renderNotUnderStoodCSV ((mapMaybe (formatNU workers) $ catMaybes res) ++ notUnderstood)
      let status = if numFailed == 0 then "Inserted all " ++ (show numSuccess) ++ " shifts. Any not understood shifts are below"
                   else "Inserted " ++ (show numSuccess) ++ "/" ++ (show $ numSuccess + numFailed) ++ " shifts. Those that could not be inserted, as well as those that could not be understood, are below:"
      heistLocal (bindSplices [("status", textSplice $ T.pack status)
                              ,("data", textSplice $ T.pack unparsed)
                              ]) $ renderWS "work/bulk"
    _ -> renderWS "work/bulk_error"

 where filterFac ss = if pFac p then ss else filter (\s -> sUser s == uId u) ss   
       formatNU workers (Shift _ user _ start stop _ _) = do u <- find ((== user).uId) workers
                                                             return (B8.unpack $ uName u, localDay start, showTimeRange start stop)
       showTimeRange start stop = (formatTime defaultTimeLocale "%R" start) ++ "-" ++ (formatTime defaultTimeLocale "%R" stop)
         
renderNotUnderstood :: Monad m => [(String,Day,String)] -> Splice m
renderNotUnderstood = mapSplices (\(name, date, shift) -> 
                           runChildrenWithText [("name",        T.pack name)
                                               ,("date",        T.pack $ show date)
                                               ,("shift",       T.pack shift)
                                               ])

renderNotUnderStoodCSV :: [(String,Day,String)] -> String
renderNotUnderStoodCSV nus = showCSV $ rows
  where days = nub $ map (\(_,d,_) -> d) nus
        names = nub $ map (\(n,_,_) -> n) nus
        blanks = map (\n -> (n,days)) names
        rows = ("Name":(map (formatTime defaultTimeLocale "%m/%d/%Y") days)) : (map (\(n, days) -> n : (map (gets n) days)) blanks)
        gets n d = maybe "" (\(_,_,s) -> s) $ find (\(n',d',s) -> n==n' && d==d') nus

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
           
parseShifts :: [User] -> [(String,Day,String)] -> ([Shift],[(String,Day,String)])
parseShifts workers fields = (parsed,unparseable)
  where ps = map parseShift fields
        parseShift (person,day,timerange) = 
          let rp = do r <- mbRange timerange
                      p <- find ((== person) . B8.unpack . uName) workers
                      return (r,p) 
          in
          case rp of
            Nothing -> Left (person,day,timerange)
            Just ((s,e), worker) -> Right $ emptyShift {sUser = (uId worker), sStart = (mkTime day s), sStop = (mkTime day e)}
        mbRange t = either (const Nothing) Just (parse timeRange "" t)
        mkTime d t = LocalTime d (timeToTimeOfDay t)
        unparseable = lefts ps
        parsed = rights ps

data AP = AM | PM | NONE deriving Show

data TIME = KNOWN DiffTime | UNKNOWN DiffTime deriving Show

halfDay = 12*60*60
fullDay = 24*60*60

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
          minutes m = fromInteger $ toInteger (m * 60) :: DiffTime
          listToInt ls = foldr (\(x,m) t -> t + (x * m)) 0 $ zip ls (reverse $ take (length ls) $ iterate (*10) 1)

timeRange :: Parser (DiffTime,DiffTime)
timeRange = do
    start <- time
    spaces
    optional (char '-')
    spaces
    end <- time
    -- now if the times are unknown, figure out our best guess of what they should be,
    -- based on our knowledge of the domain, ie, work shifts.
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
  where check (s,e) = if s < e then return (s,e) else fail "start time after end time"  

