{-# LANGUAGE OverloadedStrings #-}

module Handlers.Shifts where
  
import Snap.Extension.Heist
  
import Text.Templating.Heist
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
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
                   , ("/edit/:id",        shiftEditH)
                   , ("/delete",          method POST $ shiftDeleteH u p)
                   , ("/requestoff/:id",  requestOffH)
                   , ("/cover/:id",       coverH)
                   ]

shiftAddH u p = do
  r <- eitherSnapForm newShiftForm "add-entry-form"
  case r of
      Left splices' -> do
        heistLocal (bindSplices (splices' ++ [("disp", textSplice "block")])) $ renderWS "work/shift/add"
      Right ns@(NewShift start stop) -> do
        insertShift (emptyShift { sUser = (uId u), sPlace = (pId p), sStart = start, sStop = stop, sRecorder = (uId u)})
        (day,daySplice) <- dayLargeSplices p u (toGregorian (localDay start))
        heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_large"
        
        {-heistLocal (bindSplices [("dayNum", textSplice $ T.pack $ show $ trd $ toGregorian $ localDay start)]) $
          renderWS "work/shift/add_success"-}
 where trd (_,_,a) = a           


data NewShift = NewShift LocalTime LocalTime deriving Show

timeTransform = transformEither (\a -> either (const $ Left "Should be like 10:00am.") Right (parse parseHour "" a))

notOverlapping :: Validator Application Text NewShift
notOverlapping = checkM "Overlaps with another shift." $ \(NewShift start end) -> 
  do muid <- authenticatedUserId
     case muid of
       Nothing -> return False -- no user
       Just (UserId uid) -> checkShiftTime uid start end

timeRangeForm = (`transform` goodTime) $ (,)
  <$> input "start" Nothing `transform` timeTransform <++ errors 
  <*> input "stop" Nothing `transform` timeTransform  <++ errors
    where goodTime = transformEither (\(start,end) -> maybe (Left "End before start.") Right (guessTime start end))

newShiftForm :: SnapForm Application Text HeistView NewShift
newShiftForm = (`validate` notOverlapping)  $ (<++ errors) $ mkNS
    <$> timeRangeForm
    <*> inputRead "day" "Internal error. Email help@housetab.org" Nothing  <++ errors 
    <*> inputRead "month" "Internal error. Email help@housetab.org" Nothing  <++ errors 
    <*> inputRead "year" "Internal error. Email help@housetab.org" Nothing  <++ errors 
  where mkNS (start,stop) d m y = NewShift (LocalTime day (timeToTimeOfDay start)) (LocalTime day (timeToTimeOfDay stop))
          where day = fromGregorian y m d
  
  
shiftEditH = undefined

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

