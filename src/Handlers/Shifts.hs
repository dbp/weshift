{-# LANGUAGE OverloadedStrings #-}

module Handlers.Shifts where
  
import Snap.Snaplet.Heist
  
import Text.Templating.Heist
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Text  (Text)

import Data.Time.Calendar
import Data.Time.LocalTime

import Text.Digestive
import Text.Digestive.Heist
import Text.Digestive.Snap hiding (method)

import Control.Monad.Trans (liftIO, lift)
import Control.Applicative

import Text.Parsec hiding (Error)
  
import Snap.Core
import Application
import State.Types
import State.Shifts
import State.Account
import Handlers.Month
import Auth
import Common
import Time
import Mail (mailRequestOff, mailShiftCovered)

shiftH :: User -> UserPlace -> AppHandler ()
shiftH u p = route [ ("/add",             shiftAddH u p)
                   , ("/edit",            shiftEditH u p)
                   , ("/delete",          method POST $ shiftDeleteH u p)
                   , ("/requestoff",      method POST $ requestOffH u p)
                   , ("/unrequestoff",    method POST $ unRequestOffH u p)
                   , ("/cover",           method POST $ coverH u p)
                   ]

shiftAddH u p = do
  (view, result) <- runForm "add-shift-form" addShiftForm
  muid <- getParam "user" 
  case (result,muid) of
      (Just (ShiftTime start stop), Just uid) -> do
        -- if they are a facilitator, then accept whatever id they gave, otherwise, enforce it being theirs
        let suser = if pFac p then uid else (uId u)
        insertShift (emptyShift { sUser = suser, sPlace = (pId p), sStart = start, sStop = stop, sRecorder = (uId u)})
        (day,daySplice) <- dayLargeSplices p u (toGregorian (localDay start))
        heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_large"
      (Nothing,_) -> do
        heistLocal (bindSplices [("disp", textSplice "block")]) $ heistLocal (bindDigestiveSplices view) $ renderWS "work/shift/add"
 where trd (_,_,a) = a           


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

  
  
shiftEditH u p = do
    (view, result) <- runForm "edit-shift-form" changeShiftForm
    case result of
        Nothing -> do
          heistLocal (bindSplices [("disp", textSplice "block")]) $ heistLocal (bindDigestiveSplices view) $ renderWS "work/shift/edit"
        Just (id', ShiftTime start stop) -> do
          s <- getUserShift (uId u) id'
          case s of
            Nothing -> heistLocal (bindSplices [("id", textSplice $ TE.decodeUtf8 id'), ("disp", textSplice "block"), ("message", textSplice "Could not find shift.")]) $ renderWS "work/shift/edit_error"
            Just shift -> do
              changeShift u shift start stop 
              (day,daySplice) <- dayLargeSplices p u (toGregorian (localDay start))
              heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_large"
   where trd (_,_,a) = a


--changeShiftForm :: SnapForm AppHandler Text HeistView (BS.ByteString, ShiftTime)
changeShiftForm = notOverlappingChange $ mkS
    <$> "id" .: stringRead "Internal error S. Email help@weshift.org" Nothing
    <*> newShiftForm
  where mkS i st = (B8.pack $ show (i :: Int), st)

  

shiftDeleteH u p = do
  mid <- getParam "shift"
  case mid of
    Nothing -> redirPlaceHomeAsync
    Just id' -> do
      s <- getUserShift (uId u) id'
      case s of
        Nothing -> heistLocal (bindSplices [("id", textSplice $ TE.decodeUtf8 id'), ("disp", textSplice "block"), ("message", textSplice "Could not find shift.")]) $ renderWS "work/shift/delete_error"
        Just shift -> do
          deleteShift u shift
          (day,daySplice) <- dayLargeSplices p u (toGregorian (localDay (sStart shift)))
          heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_large"
       
requestOffH u p = do
  mid <- getParam "shift"
  case mid of
    Nothing -> redirPlaceHomeAsync
    Just id' -> do
      s <- getUserShift (uId u) id'
      case s of
        Nothing -> err id' "Could not find shift."
        Just shift -> do
          mreqid <- requestShift u shift
          case mreqid of
            (Just reqid) -> do
              -- now email anyone who can cover the shift
              aus <- getAvailableUsers shift p -- ie, people who don't have another shift at that time
              tokensEmails <- mapM (\u -> getUserEmails u >>= (\ems -> return (uToken u, map emAddress $ filter emConfirmed ems))) aus
              mailRequestOff shift reqid (uName u) p tokensEmails
              (day,daySplice) <- dayLargeSplices p u (toGregorian (localDay (sStart shift)))
              heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_large"
            Nothing -> err id' "Couldn't request off, try again or contact help@weshift.org"
 where err id' m = heistLocal (bindSplices [("id", textSplice $ TE.decodeUtf8 id'), ("disp", textSplice "block"), ("message", textSplice m)]) $ renderWS "work/shift/request_error"

unRequestOffH u p = do
  mid <- getParam "shift"
  mreq <- getParam "reqid"
  case (mid,mreq) of
    (Just id',Just reqid) -> do
      r <- getShiftRequest (uId u) id'
      s <- getUserShift (uId u) id'
      case (r,s) of
        (Just req,Just shift) -> do
          unRequestShift req
          (day,daySplice) <- dayLargeSplices p u (toGregorian (localDay (sStart shift)))
          heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_large"
        _ -> heistLocal (bindSplices [("id", textSplice $ TE.decodeUtf8 id'), ("disp", textSplice "block"), ("message", textSplice "Could not find shift.")]) $ renderWS "work/shift/unrequest_error"
        
    _ -> redirPlaceHomeAsync

-- | this is the non-ajax handler that is accessible without being logged it, suitable for emailing
coverShiftH = do
  mutok <- getParam "t" -- the user's token
  mreqid <- getParam "r" -- the id of the shift request
  muser <- maybe (return Nothing) getUserFromToken mutok
  case (muser, mreqid) of
    (Just u, Just reqid) -> do
      mshift <- getShiftByRequest reqid
      case mshift of
        (Just shift) -> do
          result <- coverShift (uId u) shift reqid
          mcuru <- getUser (sUser shift)
          emails <- maybe (return []) getUserEmails mcuru
          mailShiftCovered u shift (map emAddress $ filter emConfirmed emails)
          case result of
            False -> err "Shift overlaps with one of yours."
            True -> do 
              unRequestShift reqid
              err "Successfully covered shift. If they have an email address, we've contacted them, but you should do so anyway!"
        Nothing -> err "Could not find shift."
    _ -> pass
 where err m = heistLocal (bindSplices [("message", textSplice m)]) $ renderWS "generic_error"

       
coverH u p = do
  muid <- getParam "user"
  mid <- getParam "shift"
  mreq <- getParam "req"
  case (muid,mid,mreq) of
    (Just uid,Just id',Just reqid) -> do
      r <- getShiftRequest uid id'
      s <- getUserShift uid id'
      case (r,s) of
        (Just req,Just shift) -> do
          result <- coverShift (uId u) shift req 
          case result of
            False -> err id' "Shift overlaps with one of yours."
            True -> do 
              unRequestShift req
              (day,daySplice) <- dayLargeSplices p u (toGregorian (localDay (sStart shift)))
              heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_large"
        _ -> err id' "Could not find shift."
    _ -> redirPlaceHomeAsync
 where err i msg = heistLocal (bindSplices [("id", textSplice $ TE.decodeUtf8 i), ("disp", textSplice "block"), ("message", textSplice msg)]) $ renderWS "work/shift/cover_error"
