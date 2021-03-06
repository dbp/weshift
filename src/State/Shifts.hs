{-# LANGUAGE OverloadedStrings #-}

module State.Shifts where


import           Control.Monad
import           Data.Maybe          (catMaybes, listToMaybe, mapMaybe)
import           Data.Text           (Text)
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Database.HDBC

import           Application
import           State.Account       (buildEmail, buildUser)
import           State.Types



-- | if successful, returns Right id, else Left shift that could not be entered
insertShift :: Shift -> AppHandler (Either Shift Text)
insertShift s@(Shift _ u p start stop color units _ recorder deadline deadline_done description _) =
  fmap (\r -> if not (null r) && not (null $ head r) then Right (fromSql $ head $ head r) else Left s) $
    withPGDB "INSERT INTO shifts (place, user_id, start, stop, color, units, recorder, deadline, deadline_done, description) (SELECT ? as place, ? as user_id, ? as start, ? as stop, ? as color, ? as units, ? as recorder, ? as deadline, ? as deadline_done, ? as description) RETURNING id;" [toSql p,toSql u, toSql start, toSql stop, toSql (colorToInt color), toSql units, toSql recorder, toSql deadline, toSql deadline_done, toSql description]

-- | note: this does not actually delete a shift, it merely marks it as deleted.
deleteShift :: User -> Shift -> AppHandler Bool
deleteShift u s = fmap (not.null) $ withPGDB "INSERT INTO shiftdeletes (place, user_id, old_shift, recorder) VALUES (?, ?, ?, ?) RETURNING old_shift;" [toSql (sPlace s), toSql (sUser s), toSql (sId s), toSql (uId u)]

deadlineDone :: User -> Shift -> AppHandler Bool
deadlineDone u s = fmap (not.null) $ withPGDB "UPDATE shifts SET deadline_done = true WHERE id = ? RETURNING id;" [toSql (sId s)]

-- | note: this does not actually change a shift in place, it merely adds a change to the database.
changeShift :: User -> Shift -> LocalTime -> LocalTime -> Color -> Double -> Text -> AppHandler Bool
changeShift u s ns ne co uns desc = fmap (not.null) $ withPGDB "INSERT INTO shiftchanges (place, user_id, old_shift, start, stop, color, units, recorder, description) (SELECT ? as place, ? as user_id, ? as old_shift, ? as start, ? as stop, ? as color, ? as units, ? as recorder, ? as description) RETURNING id;" [toSql (sPlace s), toSql (sUser s), toSql (sId s), toSql ns, toSql ne, toSql (colorToInt co), toSql uns, toSql (uId u), toSql desc]

requestShift :: User -> Shift -> AppHandler (Maybe Text)
requestShift u shift = fmap ((fmap (fromSql.head)) . listToMaybe) $ withPGDB "INSERT INTO shiftrequests (shift_id, requester) VALUES (?, ?) RETURNING id;" [toSql (sId shift), toSql (uId u)]

unRequestShift :: Text -> AppHandler Bool
unRequestShift reqid = fmap (not.null) $ withPGDB "DELETE FROM shiftrequests WHERE id = ? RETURNING id;" [toSql reqid]

claimShift :: Claim -> AppHandler Bool
claimShift (Claim _ shift user units reason _ _) = fmap (not.null) $ withPGDB "INSERT INTO shiftclaims (shift_id, user_id, units, reason) VALUES (?, ?, ?, ?) RETURNING id;" [toSql shift, toSql user, toSql units, toSql reason]

resolveClaim :: Claim -> AppHandler Bool
resolveClaim (Claim id' _ _ _ _ _ ac) = fmap (not.null) $ withPGDB "UPDATE shiftclaims SET resolved = true, accepted = ? WHERE id = ? RETURNING id;" [toSql ac, toSql id']

-- | This get's all users who might be able to cover a shift. Their list of places are not complete (only have the current place), because at this point they are not needed, and it simplifies things
getAvailableUsers :: Shift -> UserPlace -> AppHandler [User]
getAvailableUsers shift place = do
  us <- withPGDB "SELECT U.id, U.name, U.active, U.super, U.view, U.token FROM users AS U JOIN placeusers AS PU ON PU.user_id = U.id AND PU.place = ? WHERE NOT EXISTS (SELECT C.id FROM shifts_current AS C WHERE C.user_id = U.id AND (C.start < ? AND C.stop > ?));" [toSql (pId place), toSql (sStop shift), toSql (sStart shift)]
  return $ mapMaybe ((fmap (\u -> u {uPlaces = [place]})) . (flip buildUser [])) us

getShiftRequest :: Text -> Text -> AppHandler (Maybe Text)
getShiftRequest uid shiftid = fmap ((fmap fromSql) . (>>= listToMaybe) . listToMaybe) $ withPGDB "SELECT R.id FROM shiftrequests AS R JOIN shifts_current AS C ON R.shift_id = C.id WHERE R.shift_id = ? AND C.user_id = ?;" [toSql shiftid, toSql uid]

getShiftByRequest :: Text -> AppHandler (Maybe Shift)
getShiftByRequest rid = fmap ((>>= buildShift).listToMaybe) $ withPGDB "SELECT S.id, S.user_id, S.place, S.start, S.stop, S.recorded, S.recorder, S.color, S.units, S.deadline, S.deadline_done, S.description, S.has_claims FROM shifts_current AS S JOIN shiftrequests as R ON S.id = R.shift_id WHERE R.id = ?;" [toSql rid]

coverShift :: Text -> Shift -> Text -> AppHandler Bool
coverShift uid shift reqid = fmap (not.null) $ withPGDB "INSERT INTO shiftcovers (shift_id, coverer) (SELECT ? as shift_id, ? as coverer WHERE NOT EXISTS (SELECT id, user_id, start, stop FROM shifts_current WHERE user_id = ? AND (? < stop AND ? > start) UNION ALL select id, user_id, start, stop FROM obligations WHERE user_id = ? AND (? < stop AND ? > start)) AND EXISTS (SELECT id FROM shiftrequests WHERE id = ?)) RETURNING id;" [toSql (sId shift), toSql uid, toSql uid, toSql (sStart shift), toSql (sStop shift), toSql uid, toSql (sStart shift), toSql (sStop shift), toSql reqid]

checkShiftTime :: Text -> LocalTime -> LocalTime -> AppHandler Bool
checkShiftTime uid start stop = fmap null $ withPGDB "SELECT id FROM shifts_current WHERE user_id = ? AND (? < stop AND ? > start) AND deadline = false" [toSql uid, toSql start, toSql stop]

checkShiftTimeExcept :: Text -> Text -> LocalTime -> LocalTime -> AppHandler Bool
checkShiftTimeExcept skip uid start stop = fmap null $ withPGDB "SELECT id FROM shifts_current WHERE user_id = ? AND (? < stop AND ? > start) AND deadline = false AND id != ?;" [toSql uid, toSql start, toSql stop, toSql skip]

getNextShift :: User -> UserPlace -> AppHandler (Maybe Shift)
getNextShift u p =
  fmap ((>>= buildShift).listToMaybe) $ withPGDB "SELECT id, user_id, place, start, stop, recorded, recorder, color, units, deadline, deadline_done, description, has_claims FROM shifts_current WHERE user_id = ? AND place = ? AND start > now() AND deadline = false ORDER BY start ASC LIMIT 1;" [toSql $ uId u, toSql $ pId p]

getNextDeadline :: User -> UserPlace -> AppHandler (Maybe Shift)
getNextDeadline u p =
  fmap ((>>= buildShift).listToMaybe) $ withPGDB "SELECT id, user_id, place, start, stop, recorded, recorder, color, units, deadline, deadline_done, description, has_claims FROM shifts_current WHERE user_id = ? AND place = ? AND start > now() AND deadline = true AND deadline_done = false ORDER BY start ASC LIMIT 1;" [toSql $ uId u, toSql $ pId p]


getShifts :: Day -> Day -> UserPlace -> AppHandler [Shift]
getShifts start end p = fmap (catMaybes . (map buildShift)) $ withPGDB "SELECT id, user_id, place, start, stop, recorded, recorder, color, units, deadline, deadline_done, description, has_claims FROM shifts_current WHERE place = ? AND start >= ? AND start < ? ORDER BY start;" [toSql $ pId p, toSql start, toSql end]

getUncoveredShifts :: Day -> Day -> UserPlace -> AppHandler [Shift]
getUncoveredShifts start end p = fmap (catMaybes . (map buildShift)) $ withPGDB "SELECT C.id, C.user_id, C.place, C.start, C.stop, C.recorded, C.recorder, C.color, C.units, C.deadline, C.deadline_done, C.description, C.has_claims FROM shiftrequests AS R JOIN shifts_current as C ON shift_id = C.id WHERE C.start > now() AND C.start >= ? AND C.start < ? AND C.place = ?;" [toSql start, toSql end, toSql $ pId p]


getOriginalShifts :: UserPlace -> Day -> Day -> AppHandler [Shift]
getOriginalShifts place start stop = fmap (catMaybes . (map buildShift) . (map (\s -> s ++ [toSql False]))) $ withPGDB "SELECT S.id, S.user_id, S.place, S.start, S.stop, S.recorded, S.recorder, S.color, S.units, S.deadline, S.deadline_done, S.description FROM shifts AS S WHERE S.place = ? AND S.start > ? AND S.stop < ?;" [toSql $ pId place, toSql start, toSql stop]

getUserCurrentShifts :: UserPlace -> User -> Day -> Day -> AppHandler [Shift]
getUserCurrentShifts place user start stop = fmap (catMaybes . (map buildShift)) $ withPGDB "SELECT S.id, S.user_id, S.place, S.start, S.stop, S.recorded, S.recorder, S.color, S.units, S.deadline, S.deadline_done, S.description, S.has_claims FROM shifts_current AS S WHERE S.place = ? AND S.user_id = ? AND S.start > ? AND S.stop < ?;" [toSql $ pId place, toSql $ uId user, toSql start, toSql stop]

getUserShift :: Text -> Text -> AppHandler (Maybe Shift)
getUserShift uid id' = fmap ((>>= buildShift).listToMaybe) $ withPGDB "SELECT S.id, S.user_id, S.place, S.start, S.stop, S.recorded, S.recorder, S.color, S.units, S.deadline, S.deadline_done, S.description, S.has_claims FROM shifts_current AS S WHERE S.id = ? AND S.user_id = ?;" [toSql $ id', toSql uid]

getShift :: Text -> UserPlace -> AppHandler (Maybe Shift)
getShift id' p = fmap ((>>= buildShift).listToMaybe) $ withPGDB "SELECT S.id, S.user_id, S.place, S.start, S.stop, S.recorded, S.recorder, S.color, S.units, S.deadline, S.deadline_done, S.description, S.has_claims FROM shifts_current AS S WHERE S.id = ? AND S.place = ?;" [toSql id', toSql (pId p)]

-- | this version does not check a place, so care should be used to as not to create vulnerabilities
getShift' :: Text -> AppHandler (Maybe Shift)
getShift' id' = fmap ((>>= buildShift).listToMaybe) $ withPGDB "SELECT S.id, S.user_id, S.place, S.start, S.stop, S.recorded, S.recorder, S.color, S.units, S.deadline, S.deadline_done, S.description, S.has_claims FROM shifts_current AS S WHERE S.id = ?;" [toSql id']

getShiftClaim :: Text -> AppHandler (Maybe Claim)
getShiftClaim id' = fmap ((>>= buildClaim).listToMaybe) $ withPGDB "SELECT id, shift_id, user_id, units, reason, resolved, accepted FROM shiftclaims WHERE id = ?;" [toSql id']

getShiftClaims :: Text -> AppHandler [Claim]
getShiftClaims id' = fmap (catMaybes . (map buildClaim)) $ withPGDB "SELECT id, shift_id, user_id, units, reason, resolved, accepted FROM shiftclaims WHERE shift_id = ?;" [toSql id']

getUnresolvedClaims :: UserPlace -> Day -> Day -> AppHandler [(Shift, Claim)]
getUnresolvedClaims place start stop = fmap (catMaybes . (map buildShiftClaim)) $ withPGDB "SELECT S.id, S.user_id, S.place, S.start, S.stop, S.recorded, S.recorder, S.color, S.units, S.deadline, S.deadline_done, S.description, S.has_claims, C.id, C.shift_id, C.user_id, C.units, C.reason, C.resolved, C.accepted FROM shiftclaims AS C JOIN shifts_current AS S ON S.id = C.shift_id WHERE place = ? AND S.start > ? AND S.stop < ? AND C.resolved = false;" [toSql (pId place), toSql start, toSql stop]
    where buildShiftClaim (si:su:sp:ss:st:sr:sb:sc:sun:sd:sdd:sdesc:scls:ci:cs:cus:cun:cr1:cr2:ac:[])
            = do s <- buildShift (si:su:sp:ss:st:sr:sb:sc:sun:sd:sdd:sdesc:scls:[])
                 c <- buildClaim (ci:cs:cus:cun:cr1:cr2:ac:[])
                 Just (s, c)
          buildShiftClaim _ = Nothing

getUserModifiedShifts :: UserPlace -> Day -> Day -> AppHandler [(Shift, [Modification])]
getUserModifiedShifts place start stop = do
  userCreated <- fmap (catMaybes . (map buildShift)) $ withPGDB "SELECT S.id, S.user_id, S.place, S.start, S.stop, S.recorded, S.recorder, S.color, S.units, S.deadline, S.deadline_done, S.description, S.has_claims FROM shifts_current AS S JOIN users AS U ON S.recorder = U.id JOIN placeusers AS PU ON PU.user_id = U.id WHERE PU.place = ? AND PU.facilitator = false;" [toSql (pId place)]
  userChanges <- fmap (catMaybes . (map buildChange)) $ withPGDB "SELECT U.name, S.id, S.user_id, S.place, S.start, S.stop, S.recorded, S.recorder, S.color, S.units, S.deadline, S.deadline_done, S.description, C.old_shift, C.start, C.stop, C.recorder, C.recorded, C.color, C.units, C.description FROM shiftchanges AS C JOIN users AS U ON recorder = U.id JOIN placeusers AS PU on PU.user_id = U.id JOIN shifts AS S on S.id = C.old_shift WHERE PU.place = ? AND PU.facilitator = false AND C.start > ? AND C.stop < ?;" [toSql (pId place), toSql start, toSql stop]
  return $ (map (\s -> (s,[])) userCreated) ++ userChanges
    where buildChange (n:si:su:sp:ss:st:sr:sb:sc:sun:sd:sdd:sdesc:sid:srt:stp:rer:red:co:un:d:[]) = buildShift (si:su:sp:ss:st:sr:sb:sc:sun:sd:sdd:sdesc:[]) >>= (\shift -> return (shift, [Change (fromSql srt) (fromSql stp) (colorFromInt (fromSql co)) (fromSql un) (emptyUser {uId = (fromSql rer), uName = (fromSql n)}) (fromSql red) (fromSql d)]))
          buildChange _ = Nothing

getShiftChanges :: Shift -> AppHandler [Modification]
getShiftChanges shift = fmap (catMaybes . (map buildChange)) $ withPGDB "SELECT U.name, start, stop, recorder, recorded, color, units, description FROM shiftchanges JOIN users AS U ON recorder = U.id WHERE old_shift = ?;" [toSql $ sId shift]
  where buildChange (n:srt:stp:rer:red:co:un:d:[]) = Just $ Change (fromSql srt) (fromSql stp) (colorFromInt (fromSql co)) (fromSql un) (emptyUser {uId = (fromSql rer), uName = (fromSql n)}) (fromSql red) (fromSql d)
        buildChange _ = Nothing

getShiftDeletes :: Shift -> AppHandler [Modification]
getShiftDeletes shift = fmap (catMaybes . (map buildDelete)) $ withPGDB "SELECT U.name, recorder, recorded FROM shiftdeletes JOIN users AS U ON recorder = U.id WHERE old_shift = ?;" [toSql $ sId shift]
  where buildDelete (name:rer:red:[]) = Just $ Delete (emptyUser {uId = (fromSql rer), uName = (fromSql name)}) (fromSql red)
        buildDelete _ = Nothing

getShiftCovers :: Shift -> AppHandler [Modification]
getShiftCovers shift = fmap (catMaybes . (map buildCover)) $ withPGDB "SELECT U.name, coverer, recorded FROM shiftcovers JOIN users AS U ON coverer = U.id WHERE shift_id = ?;"  [toSql $ sId shift]
  where buildCover (n:c:r:[]) = Just $ Cover (emptyUser {uId = (fromSql c), uName = (fromSql n)}) (fromSql r)
        buildCover _ = Nothing
