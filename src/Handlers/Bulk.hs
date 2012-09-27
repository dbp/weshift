{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Handlers.Bulk where
  
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
import Text.SSV
import Text.Parsec
import Data.Char (digitToInt)
import Data.Either (lefts,rights)

import State.Coworkers
import State.Shifts

import Render.Shifts
import Render.Bulk

bulkInputH :: User -> UserPlace -> AppHandler ()
bulkInputH user place = 
  route [("/", ifTop $ method GET $ inputForm user)
        ,("/upload", method POST $ upload user place)
        ,("/confirm", method POST $ confirm user place)
        ]
        
inputForm u = do
    today <- fmap utctDay $ liftIO getCurrentTime
    setView u "work" "work.bulk"
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
      let numSuccess = length $ rights res
      let numFailed = length $ lefts res
      let unparsed = renderNotUnderStoodCSV ((mapMaybe (formatNU workers) $ lefts res) ++ notUnderstood)
      let status = if numFailed == 0 then "Inserted all " ++ (show numSuccess) ++ " shifts. Any not understood shifts are below"
                   else "Inserted " ++ (show numSuccess) ++ "/" ++ (show $ numSuccess + numFailed) ++ " shifts. Those that could not be inserted (due to overlapping with existing shifts), as well as those that could not be understood, are below:"
      heistLocal (bindSplices [("status", textSplice $ T.pack status)
                              ,("data", textSplice $ T.pack unparsed)
                              ]) $ renderWS "work/bulk"
    _ -> renderWS "work/bulk_error"

 where filterFac ss = if pFac p then ss else filter (\s -> sUser s == uId u) ss   
       formatNU workers (Shift _ user _ start stop _ _ _ _ _ _ _) = 
          do u <- find ((== user).uId) workers
             return (B8.unpack $ uName u, localDay start, showTimeRange start stop)
       showTimeRange start stop = (ftime start) ++ "-" ++ (ftime stop)
       ftime t = if ftime' "%M" t == "00" then ftime' "%H" t else ftime' "%R" t
       ftime' = formatTime defaultTimeLocale

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
