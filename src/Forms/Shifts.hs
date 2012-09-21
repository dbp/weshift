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
import Text.Parsec hiding (Error, choice)
import State.Shifts

data ShiftTime = ShiftTime BS.ByteString LocalTime LocalTime Color Double deriving Show

timeTransform = validate (\a -> either (const $ Error "Should be like 10:00am.") Success (parse parseHour "" (T.unpack a)))

notOverlapping = checkM "Overlaps with another shift." $ \(ShiftTime user start end _ _) -> 
    checkShiftTime user start end

notOverlappingChange = checkM "Overlaps with another shift." $ \(skip, s@(ShiftTime user start end _ _)) -> 
    checkShiftTimeExcept skip user start end

timeRangeForm :: Form T.Text AppHandler (DiffTime, DiffTime)
timeRangeForm = goodTime $ (,)
  <$> "start" .: timeTransform (text Nothing)
  <*> "stop"  .: timeTransform (text Nothing)
    where goodTime = validate (\(start,end) -> maybe (Error "End before start.") Success (guessTime start end))

addShiftForm = notOverlapping $ newShiftForm

newShiftForm = mkNS
    <$> timeRangeForm
    <*> "user"  .: stringRead "Internal error U. Email help@weshift.org" Nothing 
    <*> "color" .: stringRead "Internal error C. Email help@weshift.org" Nothing
    <*> "units" .: stringRead "Must be a number like 1.5" Nothing
    <*> "day"   .: stringRead "Internal error D. Email help@weshift.org" Nothing
    <*> "month" .: stringRead "Internal error M. Email help@weshift.org" Nothing 
    <*> "year"  .: stringRead "Internal error Y. Email help@weshift.org" Nothing
  where mkNS (start,stop) user c u d m y = ShiftTime (B8.pack $ show (user :: Int))
                                                     (LocalTime day (timeToTimeOfDay start)) 
                                                     (LocalTime day (timeToTimeOfDay stop)) c u
          where day = fromGregorian y m d

--changeShiftForm :: SnapForm AppHandler Text HeistView (BS.ByteString, ShiftTime)
changeShiftForm = notOverlappingChange $ mkS
    <$> "id" .: stringRead "Internal error S. Email help@weshift.org" Nothing
    <*> newShiftForm
  where mkS i st = (B8.pack $ show (i :: Int), st)

  