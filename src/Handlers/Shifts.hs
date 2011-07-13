{-# LANGUAGE OverloadedStrings #-}

module Handlers.Shifts where
  
import Snap.Extension.Heist
  
import Text.Templating.Heist
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Text  (Text)

import Data.Time.Calendar
import Data.Time.LocalTime

import Text.Digestive.Types
import Text.Digestive.Snap.Heist
import Text.Digestive.Validate
import Text.Digestive.Transform

import Control.Monad.Trans (liftIO, lift)
import Control.Applicative

import Text.Parsec
  
import Snap.Types
import Snap.Auth
import Application
import State.Types
import State.Shifts
import Handlers.Month
import Common
import Time

shiftH :: User -> UserPlace -> Application ()
shiftH u p = route [ ("/add",             shiftAddH u p)
                   , ("/edit",            shiftEditH u p)
                   , ("/delete",          method POST $ shiftDeleteH u p)
                   , ("/requestoff/:id",  requestOffH)
                   , ("/cover/:id",       coverH)
                   ]

shiftAddH u p = do
  r <- eitherSnapForm addShiftForm "add-shift-form"
  case r of
      Left splices' -> do
        heistLocal (bindSplices (splices' ++ [("disp", textSplice "block")])) $ renderWS "work/shift/add"
      Right (ShiftTime start stop) -> do
        insertShift (emptyShift { sUser = (uId u), sPlace = (pId p), sStart = start, sStop = stop, sRecorder = (uId u)})
        (day,daySplice) <- dayLargeSplices p u (toGregorian (localDay start))
        heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_large"
 where trd (_,_,a) = a           


data ShiftTime = ShiftTime LocalTime LocalTime deriving Show

timeTransform = transformEither (\a -> either (const $ Left "Should be like 10:00am.") Right (parse parseHour "" a))

notOverlapping :: Validator Application Text ShiftTime
notOverlapping = checkM "Overlaps with another shift." $ \(ShiftTime start end) -> 
  do muid <- authenticatedUserId
     case muid of
       Nothing -> return False -- no user
       Just (UserId uid) -> checkShiftTime uid start end

notOverlappingChange :: Validator Application Text (BS.ByteString, ShiftTime)
notOverlappingChange = checkM "Overlaps with another shift." $ \(skip, s@(ShiftTime start end)) -> 
  do muid <- authenticatedUserId
     case muid of
       Nothing -> return False -- no user
       Just (UserId uid) -> checkShiftTimeExcept skip uid start end

timeRangeForm = (`transform` goodTime) $ (<++ childErrors) $ (,)
  <$> input "start" Nothing `transform` timeTransform
  <*> input "stop"  Nothing `transform` timeTransform
    where goodTime = transformEither (\(start,end) -> maybe (Left "End before start.") Right (guessTime start end))

addShiftForm = (`validate` notOverlapping)  $ (<++ childErrors) $ newShiftForm

newShiftForm :: SnapForm Application Text HeistView ShiftTime
newShiftForm = mkNS
    <$> timeRangeForm
    <*> inputRead "day" "Internal error D. Email help@housetab.org" Nothing
    <*> inputRead "month" "Internal error M. Email help@housetab.org" Nothing 
    <*> inputRead "year" "Internal error Y. Email help@housetab.org" Nothing
  where mkNS (start,stop) d m y = ShiftTime (LocalTime day (timeToTimeOfDay start)) (LocalTime day (timeToTimeOfDay stop))
          where day = fromGregorian y m d

  
  
shiftEditH u p = do
    r <- eitherSnapForm changeShiftForm "edit-shift-form"
    case r of
        Left splices' -> do
          heistLocal (bindSplices (splices' ++ [("disp", textSplice "block")])) $ renderWS "work/shift/edit"
        Right (id', ShiftTime start stop) -> do
          s <- getUserShift u id'
          case s of
            Nothing -> heistLocal (bindSplices [("id", textSplice $ TE.decodeUtf8 id'), ("disp", textSplice "block"), ("message", textSplice "Could not find shift.")]) $ renderWS "work/shift/edit_error"
            Just shift -> do
              changeShift u shift start stop 
              (day,daySplice) <- dayLargeSplices p u (toGregorian (localDay start))
              heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_large"
   where trd (_,_,a) = a


changeShiftForm :: SnapForm Application Text HeistView (BS.ByteString, ShiftTime)
changeShiftForm = (`validate` notOverlappingChange)  $ (<++ childErrors) $ mkS
    <$> inputRead "id" "Internal error S. Email help@housetab.org" Nothing
    <*> newShiftForm
  where mkS i st = (B8.pack $ show (i :: Int), st)

  

shiftDeleteH u p = do
  mid <- getParam "shift"
  case mid of
    Nothing -> redirPlaceHomeAsync
    Just id' -> do
      s <- getUserShift u id'
      case s of
        Nothing -> heistLocal (bindSplices [("id", textSplice $ TE.decodeUtf8 id'), ("disp", textSplice "block"), ("message", textSplice "Could not find shift.")]) $ renderWS "work/shift/delete_error"
        Just shift -> do
          deleteShift u shift
          (day,daySplice) <- dayLargeSplices p u (toGregorian (localDay (sStart shift)))
          heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_large"
       
requestOffH = undefined
coverH = undefined

