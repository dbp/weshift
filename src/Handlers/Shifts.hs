{-# LANGUAGE OverloadedStrings #-}

module Handlers.Shifts where
  
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
import            State.Shifts
import            State.Account
import            Render.Calendar
import            Mail (mailRequestOff, mailShiftCovered, mailClaim)
import            Forms.Shifts

shiftH :: User -> UserPlace -> AppHandler ()
shiftH u p = route [ ("/add",               shiftAddH u p)
                   , ("/edit",              shiftEditH u p)
                   , ("/split",             shiftSplitH u p)
                   , ("/claim/:id/accept",  shiftClaimAcceptH u p)
                   , ("/claim/:id/cancel",  shiftClaimCancelH u p)
                   , ("/claim",             shiftClaimH u p)
                   , ("/delete",            method POST $ shiftDeleteH u p)
                   , ("/requestoff",        method POST $ requestOffH u p)
                   , ("/unrequestoff",      method POST $ unRequestOffH u p)
                   , ("/cover",             method POST $ coverH u p)
                   , ("/deadline_done",     method POST $ deadlineDoneH u p)
                   ]

shiftAddH u p = do
  (view, result) <- wsForm addShiftForm
  case result of
      Just (ShiftTime uid start stop color units description)-> do
        -- if they are a facilitator, then accept whatever id they gave, otherwise, enforce it being theirs
        let suser = if pFac p then uid else (uId u)
        isDeadline <- fmap (maybe False (const True)) $ getParam "ws.deadline"
        insertShift (emptyShift { sUser = suser, sPlace = (pId p), sStart = start, sStop = stop, 
                                  sRecorder = (uId u), sColor = color, sUnits = units, 
                                  sDeadline = isDeadline, sDescription = description})
        (day,daySplice) <- dayLargeSplices p u (toGregorian (localDay start))
        heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_large"
      Nothing -> do
        dayNum <- fmap (fromMaybe "") $ getParam "ws.day"
        heistLocal (bindSplices [("disp", textSplice "block"), ("dayNum", textSplice $ TE.decodeUtf8 dayNum)]) $ heistLocal (bindDigestiveSplices view) $ renderWS "work/shift/add"
 where trd (_,_,a) = a           

  
  
shiftEditH u p = do
    id' <- fmap (TE.decodeUtf8.fromJust) $ getParam "ws.id" -- if this isn't present, it means tampering, so don't care.
    (view, result) <- wsForm changeShiftForm
    case result of
        Nothing -> do
          heistLocal (bindSplices [("disp", textSplice "block"), ("id", textSplice id')]) $ heistLocal (bindDigestiveSplices view) $ renderWS "work/shift/edit"
        Just (id', ShiftTime user start stop color units description) -> do
          let suser = if pFac p then user else (uId u)
          -- this is guaranteed to fail for non-facilitators editing other people's shifts
          s <- getUserShift suser id'
          case s of
            Nothing -> do
              dayNum <- fmap (fromMaybe "") $ getParam "ws.day"
              heistLocal (bindSplices [("id", textSplice $ TE.decodeUtf8 id'), 
                                       ("disp", textSplice "block"), 
                                       ("message", textSplice "Could not find shift."),
                                       ("dayNum", textSplice $ TE.decodeUtf8 dayNum)]) $ renderWS "work/shift/edit_error"
            Just shift -> do
              success <- changeShift u shift start stop color units description
              case success of
                True -> do
                  (day,daySplice) <- dayLargeSplices p u (toGregorian (localDay start))
                  heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_large"
                False -> do
                  dayNum <- fmap (fromMaybe "") $ getParam "ws.day"
                  heistLocal (bindSplices [("id", textSplice $ TE.decodeUtf8 id'), 
                                       ("disp", textSplice "block"), 
                                       ("message", textSplice "Error E, Contact help@weshift.org"),
                                       ("dayNum", textSplice $ TE.decodeUtf8 dayNum)]) $ renderWS "work/shift/edit_error"
   where trd (_,_,a) = a

shiftClaimH u p = do
  id' <- fmap (TE.decodeUtf8.fromJust) $ getParam "ws.id" -- if this isn't present, it means tampering, so don't care.
  (view, result) <- wsForm claimShiftForm
  case result of
    Nothing -> heistLocal (bindSplices [("disp", textSplice "block"), ("id", textSplice id')]) $ heistLocal (bindDigestiveSplices view) $ renderWS "work/shift/claim"
    Just (id', userid, units, reason) -> do
      let suser = if pFac p then userid else (uId u)
      -- we want to guarantee that the shift is at this place, in case of tampering
      shift' <- getShift id' p
      user' <- getUser userid
      case (shift', user') of
        (Just shift, Just user) -> do
          success <- claimShift (emptyClaim {cShift = (sId shift), cUser = suser, cUnits = units, cReason = reason})
          case success of
            True -> do
              target' <- getUser (sUser shift)
              case target' of
                Nothing -> heistLocal (bindSplices [("id", textSplice $ TE.decodeUtf8 id'), ("disp", textSplice "block"), ("message", textSplice "Error U. Contact help@weshift.org")]) $ renderWS "work/shift/claim_error"
                Just target -> do
                  emails <- getUserEmails target
                  mapM (\email -> mailClaim (uName target) (uName user) (emAddress email) (B8.pack $ show units) reason (U.wsTimeStamp $ sStart shift)) emails
                  (day,daySplice) <- dayLargeSplices p u (toGregorian (localDay (sStart shift)))
                  heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_large"
            False -> heistLocal (bindSplices [("id", textSplice $ TE.decodeUtf8 id'), ("disp", textSplice "block"), ("message", textSplice "Error C. Contact help@weshift.org")]) $renderWS "work/shift/claim_error"
        _ -> heistLocal (bindSplices [("id", textSplice $ TE.decodeUtf8 id'), ("disp", textSplice "block"), ("message", textSplice "Could not find shift.")]) $ renderWS "work/shift/claim_error"

shiftClaimCancelH u p = do
  id' <- fmap fromJust $ getParam "id" -- if this isn't present, it means tampering, so don't care.
  claim' <- getShiftClaim id'
  case claim' of
    Nothing -> heistLocal (bindSplices [("id", textSplice $ TE.decodeUtf8 id'), ("message", textSplice "Could not find claim.")]) $ renderWS "work/shift/claims_error"
    Just claim -> do
      -- postgres guarantees that while a claim exists, its corresponding shift exists
      shift <- fmap fromJust $ getShift (cShift claim) p
      resolveClaim (claim {cAccepted = False})
      (day,daySplice) <- dayLargeSplices p u (toGregorian (localDay (sStart shift)))
      heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_large"

shiftClaimAcceptH u p = do
  id' <- fmap fromJust $ getParam "id" -- if this isn't present, it means tampering, so don't care.
  claim' <- getShiftClaim id'
  case claim' of
    Nothing -> heistLocal (bindSplices [("id", textSplice $ TE.decodeUtf8 id'), ("message", textSplice "Could not find claim.")]) $ renderWS "work/shift/claims_error"
    Just claim -> do
      -- postgres guarantees that while a claim exists, its corresponding shift exists
      shift <- fmap fromJust $ getShift (cShift claim) p
      -- we create a change to take away the units, and then create a deadline based
      -- shift for the other person, to give them the units. we use deadlines so that
      -- it can overlap
      changeShift u shift (sStart shift) (sStop shift) (sColor shift) (sUnits shift - cUnits claim) (sDescription shift)
      insertShift (emptyShift { sStart = (sStart shift), sStop = (sStop shift), 
                                sDeadline = True, sDeadlineDone = True, 
                                sDescription = (cReason claim), sUnits = (cUnits claim), 
                                sUser = (cUser claim), sPlace = (pId p), sRecorder = (uId u)})
      resolveClaim (claim {cAccepted = True})
      (day,daySplice) <- dayLargeSplices p u (toGregorian (localDay (sStart shift)))
      heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_large"


shiftSplitH u p = do
    id' <- fmap (TE.decodeUtf8.fromJust) $ getParam "ws.id" -- if this isn't present, it means tampering, so don't care.
    (view, result) <- wsForm splitShiftForm
    case result of
        Nothing -> do
          heistLocal (bindSplices [("disp", textSplice "block"), ("id", textSplice id')]) $ heistLocal (bindDigestiveSplices view) $ renderWS "work/shift/split"
        Just (id', ShiftTime user start stop color units description) -> do
          let suser = if pFac p then user else (uId u)
          -- this is guaranteed to fail for non-facilitators editing other people's shifts
          s <- getUserShift suser id'
          dayNum <- fmap (fromMaybe "") $ getParam "ws.day"
          case s of
            Nothing -> retErr id' dayNum "Could not find shift."
            Just shift -> do
              success <- changeShift u shift start stop color units description
              case success of
                True -> do
                  insertShift (shift { sUser = suser, sPlace = (pId p), sStart = stop, 
                                       sRecorder = (uId u), sUnits = ((sUnits shift) - units) })
                  (day,daySplice) <- dayLargeSplices p u (toGregorian (localDay start))
                  heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_large"
                False -> retErr id' dayNum "Error E, Contact help@weshift.org"
   where trd (_,_,a) = a
         retErr id' dayNum msg = 
              heistLocal (bindSplices [("id", textSplice $ TE.decodeUtf8 id'), 
                                       ("disp", textSplice "block"), 
                                       ("message", textSplice msg),
                                       ("dayNum", textSplice $ TE.decodeUtf8 dayNum)]) $ renderWS "work/shift/split_error"



shiftDeleteH u p = do
  mid <- getParam "shift"
  case mid of
    Nothing -> redirPlaceHomeAsync
    Just id' -> do
      s <- if (pFac p) then getShift id' p else getUserShift (uId u) id'
      case s of
        Nothing -> heistLocal (bindSplices [("id", textSplice $ TE.decodeUtf8 id'), ("disp", textSplice "block"), ("message", textSplice "Could not find shift.")]) $ renderWS "work/shift/delete_error"
        Just shift -> do
          deleteShift u shift
          (day,daySplice) <- dayLargeSplices p u (toGregorian (localDay (sStart shift)))
          heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_large"

deadlineDoneH u p = do
  mid <- getParam "shift"
  case mid of
    Nothing -> redirPlaceHomeAsync
    Just id' -> do
      s <- if (pFac p) then getShift id' p else getUserShift (uId u) id'
      case s of
        Nothing -> heistLocal (bindSplices [("id", textSplice $ TE.decodeUtf8 id'), ("disp", textSplice "block"), ("message", textSplice "Could not find shift.")]) $ renderWS "work/shift/deadline_done_error"
        Just shift -> do
          deadlineDone u shift
          (day,daySplice) <- dayLargeSplices p u (toGregorian (localDay (sStart shift)))
          heistLocal (bindSplices (daySplice ++ (commonSplices day))) $ renderWS "work/month_day_large"          
       
requestOffH u p = do
  mid <- getParam "shift"
  notify <- fmap (maybe False (const True)) $ getParam "notify"
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
              case notify of
                False -> return ()
                True -> do
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
