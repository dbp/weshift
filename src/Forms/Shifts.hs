{-# LANGUAGE OverloadedStrings #-}

module Forms.Shifts where

-- | Boilerplate imports
import qualified Data.Bson             as B
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map              as M
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Imports
import qualified Text.XmlHtml          as X
import qualified Utils                 as U

-- | Module specific imports
import           State.Shifts
import           Text.Parsec           hiding (Error, choice)

data ShiftTime = ShiftTime T.Text LocalTime LocalTime Color Double T.Text deriving Show

timeTransform = validate (\a -> either (const $ Error "Should be like 10:00am.") Success (parse parseHour "" (T.unpack a)))

notOverlapping = checkM "Overlaps with another shift." $ \(ShiftTime user start end _ _ _) ->
  do isDeadline <- fmap isJust $ getParam "ws.deadline"
     if isDeadline then return True else checkShiftTime user start end

notOverlappingChange = checkM "Overlaps with another shift." $ \(skip, s@(ShiftTime user start end _ _ _)) ->
  do isDeadline <- fmap (maybe False sDeadline) $ getShift' skip
     if isDeadline then return True else checkShiftTimeExcept skip user start end

withinRange = checkM "Stop and units must be within current shift." $ \(current, s@(ShiftTime user start end _ units _)) ->
  do shift <- getShift' current
     maybe (return False) (\s -> return (end > sStart s && end <= sStop s && units <= sUnits s)) shift

descLength = check "Description too long: maximum 30 characters" $ \t -> T.length t < 30

nonNegative = check "Units cannot be negative" $ \u -> u >= 0

timeRangeForm :: Form T.Text AppHandler (DiffTime, DiffTime)
timeRangeForm = goodTime $ (,)
  <$> "start" .: timeTransform (text Nothing)
  <*> "stop"  .: timeTransform (text Nothing)
    where goodTime = validate (\(start,end) -> maybe (Error "End before start.") Success (guessTime start end))

addShiftForm = notOverlapping $ newShiftForm

newShiftForm = mkNS
    <$> timeRangeForm
    <*> "user"        .: stringRead "Internal error U. Email help@weshift.org" Nothing
    <*> "color"       .: stringRead "Internal error C. Email help@weshift.org" Nothing
    <*> "units"       .: nonNegative (stringRead "Must be a number like 1.5" Nothing)
    <*> "day"         .: stringRead "Internal error D. Email help@weshift.org" Nothing
    <*> "month"       .: stringRead "Internal error M. Email help@weshift.org" Nothing
    <*> "year"        .: stringRead "Internal error Y. Email help@weshift.org" Nothing
    <*> "description" .: descLength (text Nothing)
  where mkNS (start,stop) user c u d m y desc  = ShiftTime (T.pack $ show (user :: Int))
                                                           (LocalTime day (timeToTimeOfDay start))
                                                           (LocalTime day (timeToTimeOfDay stop)) c u
                                                           desc
          where day = fromGregorian y m d

changeShiftForm = notOverlappingChange $ mkS
    <$> "id" .: stringRead "Internal error S. Email help@weshift.org" Nothing
    <*> newShiftForm
  where mkS i st = (T.pack $ show (i :: Int), st)

splitShiftForm = withinRange $ mkS
    <$> "id" .: stringRead "Internal error SP. Email help@weshift.org" Nothing
    <*> newShiftForm
  where mkS i st = (T.pack $ show (i :: Int), st)

unitCheck = checkM "Cannot claim more units than in the shift" $ \(shift, _, units, _) -> do
  s <- getShift' shift
  -- if the shift doesn't exist, it means tampering, so we actually want to not give a good error message
  return $ maybe False (\s' -> units <= (sUnits s')) s

claimShiftForm = unitCheck $ mkS
    <$> "id"     .: stringRead "Internal error SC. Email help@weshift.org" Nothing
    <*> "user"   .: stringRead "Internal error UC. Email help@weshift.org" Nothing
    <*> "units"  .: nonNegative (stringRead "Must be a number like 1.5" Nothing)
    <*> "reason" .: text Nothing
  where mkS i user units r = (T.pack $ show (i :: Int), T.pack $ show (user :: Int), units :: Double, r)
