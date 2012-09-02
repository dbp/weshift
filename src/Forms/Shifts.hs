{-# LANGUAGE OverloadedStrings #-}

module Forms.Shifts where

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
import Text.Parsec hiding (Error)
import State.Shifts

data ShiftTime = ShiftTime LocalTime LocalTime deriving Show

timeTransform = validate (\a -> either (const $ Error "Should be like 10:00am.") Success (parse parseHour "" (T.unpack a)))

--notOverlapping :: Validator AppHandler Text ShiftTime
notOverlapping = checkM "Overlaps with another shift." $ \(ShiftTime start end) -> 
  do muid <- authenticatedUserId
     case muid of
       Nothing -> return False -- no user
       Just uid -> checkShiftTime uid start end

--notOverlappingChange :: Validator AppHandler Text (BS.ByteString, ShiftTime)
notOverlappingChange = checkM "Overlaps with another shift." $ \(skip, s@(ShiftTime start end)) -> 
  do muid <- authenticatedUserId
     case muid of
       Nothing -> return False -- no user
       Just uid -> checkShiftTimeExcept skip uid start end

timeRangeForm :: Form T.Text AppHandler (DiffTime, DiffTime)
timeRangeForm = goodTime $ (,)
  <$> "start" .: timeTransform (text Nothing)
  <*> "stop"  .: timeTransform (text Nothing)
    where goodTime = validate (\(start,end) -> maybe (Error "End before start.") Success (guessTime start end))

addShiftForm = notOverlapping $ newShiftForm

--newShiftForm :: SnapForm AppHandler Text HeistView ShiftTime
newShiftForm = mkNS
    <$> timeRangeForm
    <*> "day"   .: stringRead "Internal error D. Email help@weshift.org" Nothing
    <*> "month" .: stringRead "Internal error M. Email help@weshift.org" Nothing 
    <*> "year"  .: stringRead "Internal error Y. Email help@weshift.org" Nothing
  where mkNS (start,stop) d m y = ShiftTime (LocalTime day (timeToTimeOfDay start)) (LocalTime day (timeToTimeOfDay stop))
          where day = fromGregorian y m d

--changeShiftForm :: SnapForm AppHandler Text HeistView (BS.ByteString, ShiftTime)
changeShiftForm = notOverlappingChange $ mkS
    <$> "id" .: stringRead "Internal error S. Email help@weshift.org" Nothing
    <*> newShiftForm
  where mkS i st = (B8.pack $ show (i :: Int), st)

  