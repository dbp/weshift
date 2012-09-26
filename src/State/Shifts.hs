{-# LANGUAGE OverloadedStrings #-}

module State.Shifts where


import            Control.Monad
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import            Database.HDBC
import            Data.Time.LocalTime
import            Data.Time.Calendar

import            Application
import            State.Types
import            State.Account (buildUser)



-- | if successful, returns Right id, else Left shift that could not be entered
insertShift :: Shift -> AppHandler (Either Shift BS.ByteString)
insertShift s@(Shift _ u p start stop color units _ recorder) = 
  fmap (\r -> if not (null r) && not (null $ head r) then Right (fromSql $ head $ head r) else Left s) $ 
    withPGDB "INSERT INTO shifts (place, user_id, start, stop, color, units, recorder) (SELECT ? as place, ? as user_id, ? as start, ? as stop, ? as color, ? as units, ? as recorder WHERE NOT EXISTS (SELECT id, user_id, start, stop, color, units FROM shifts_current WHERE user_id = ? AND (? < stop AND ? > start))) RETURNING id;" [toSql p,toSql u, toSql start, toSql stop, toSql (colorToInt color), toSql units, toSql recorder, toSql u, toSql start, toSql stop]

-- | note: this does not actually delete a shift, it merely marks it as deleted.
deleteShift :: User -> Shift -> AppHandler Bool
deleteShift u s = fmap (not.null) $ withPGDB "INSERT INTO shiftdeletes (place, user_id, old_shift, recorder) VALUES (?, ?, ?, ?) RETURNING old_shift;" [toSql (sPlace s), toSql (sUser s), toSql (sId s), toSql (uId u)]

-- | note: this does not actually change a shift in place, it merely adds a change to the database.
changeShift :: User -> Shift -> LocalTime -> LocalTime -> Color -> Double -> AppHandler Bool
changeShift u s ns ne co uns = fmap (not.null) $ withPGDB "INSERT INTO shiftchanges (place, user_id, old_shift, start, stop, color, units, recorder) (SELECT ? as place, ? as user_id, ? as old_shift, ? as start, ? as stop, ? as color, ? as units, ? as recorder WHERE NOT EXISTS (SELECT id, user_id, start, stop FROM shifts_current WHERE user_id = ? AND (? < stop AND ? > start) AND id != ?)) RETURNING id;" [toSql (sPlace s), toSql (sUser s), toSql (sId s), toSql ns, toSql ne, toSql (colorToInt co), toSql uns, toSql (uId u), toSql (sUser s), toSql ns, toSql ne, toSql (sId s)]

requestShift :: User -> Shift -> AppHandler (Maybe BS.ByteString)
requestShift u shift = fmap ((fmap (fromSql.head)) . listToMaybe) $ withPGDB "INSERT INTO shiftrequests (shift_id, requester) VALUES (?, ?) RETURNING id;" [toSql (sId shift), toSql (uId u)]

unRequestShift :: BS.ByteString -> AppHandler Bool
unRequestShift reqid = fmap (not.null) $ withPGDB "DELETE FROM shiftrequests WHERE id = ? RETURNING id;" [toSql reqid]

-- | This get's all users who might be able to cover a shift. Their list of places are not complete (only have the current place), because at this point they are not needed, and it simplifies things
getAvailableUsers :: Shift -> UserPlace -> AppHandler [User]
getAvailableUsers shift place = do
  us <- withPGDB "SELECT U.id, U.name, U.active, U.super, U.view, U.token FROM users AS U JOIN placeusers AS PU ON PU.user_id = U.id AND PU.place = ? WHERE NOT EXISTS (SELECT C.id FROM shifts_current AS C WHERE C.user_id = U.id AND (C.start < ? AND C.stop > ?));" [toSql (pId place), toSql (sStop shift), toSql (sStart shift)]
  return $ mapMaybe ((fmap (\u -> u {uPlaces = [place]})) . (flip buildUser [])) us

getShiftRequest :: BS.ByteString -> BS.ByteString -> AppHandler (Maybe BS.ByteString)
getShiftRequest uid shiftid = fmap ((fmap fromSql) . (>>= listToMaybe) . listToMaybe) $ withPGDB "SELECT R.id FROM shiftrequests AS R JOIN shifts_current AS C ON R.shift_id = C.id WHERE R.shift_id = ? AND C.user_id = ?;" [toSql shiftid, toSql uid]

getShiftByRequest :: BS.ByteString -> AppHandler (Maybe Shift)
getShiftByRequest rid = fmap ((>>= buildShift).listToMaybe) $ withPGDB "SELECT S.id, S.user_id, S.place, S.start, S.stop, S.recorded, S.recorder, S.color, S.units FROM shifts_current AS S JOIN shiftrequests as R ON S.id = R.shift_id WHERE R.id = ?;" [toSql rid]

coverShift :: BS.ByteString -> Shift -> BS.ByteString -> AppHandler Bool
coverShift uid shift reqid = fmap (not.null) $ withPGDB "INSERT INTO shiftcovers (shift_id, coverer) (SELECT ? as shift_id, ? as coverer WHERE NOT EXISTS (SELECT id, user_id, start, stop FROM shifts_current WHERE user_id = ? AND (? < stop AND ? > start) UNION ALL select id, user_id, start, stop FROM obligations WHERE user_id = ? AND (? < stop AND ? > start)) AND EXISTS (SELECT id FROM shiftrequests WHERE id = ?)) RETURNING id;" [toSql (sId shift), toSql uid, toSql uid, toSql (sStart shift), toSql (sStop shift), toSql uid, toSql (sStart shift), toSql (sStop shift), toSql reqid]

checkShiftTime :: BS.ByteString -> LocalTime -> LocalTime -> AppHandler Bool
checkShiftTime uid start stop = fmap null $ withPGDB "SELECT id FROM shifts_current WHERE user_id = ? AND (? < stop AND ? > start)" [toSql uid, toSql start, toSql stop]

checkShiftTimeExcept :: BS.ByteString -> BS.ByteString -> LocalTime -> LocalTime -> AppHandler Bool
checkShiftTimeExcept skip uid start stop = fmap null $ withPGDB "SELECT id FROM shifts_current WHERE user_id = ? AND (? < stop AND ? > start) AND id != ?;" [toSql uid, toSql start, toSql stop, toSql skip]

getNextShift :: User -> UserPlace -> AppHandler (Maybe Shift)
getNextShift u p =
  fmap ((>>= buildShift).listToMaybe) $ withPGDB "SELECT id, user_id, place, start, stop, recorded, recorder, color, units FROM shifts_current WHERE user_id = ? AND place = ? AND start > now() ORDER BY start ASC LIMIT 1;" [toSql $ uId u, toSql $ pId p]


getShifts :: Day -> Day -> UserPlace -> AppHandler [Shift]
getShifts start end p = fmap (catMaybes . (map buildShift)) $ withPGDB "SELECT id, user_id, place, start, stop, recorded, recorder, color, units FROM shifts_current WHERE place = ? AND start >= ? AND start < ? ORDER BY start;" [toSql $ pId p, toSql start, toSql end]

getUncoveredShifts :: Day -> Day -> UserPlace -> AppHandler [Shift]
getUncoveredShifts start end p = fmap (catMaybes . (map buildShift)) $ withPGDB "SELECT C.id, C.user_id, C.place, C.start, C.stop, C.recorded, C.recorder, C.color, C.units FROM shiftrequests AS R JOIN shifts_current as C ON shift_id = C.id WHERE C.start > now() AND C.start >= ? AND C.start < ? AND C.place = ?;" [toSql start, toSql end, toSql $ pId p]


getOriginalShifts :: UserPlace -> Day -> Day -> AppHandler [Shift]
getOriginalShifts place start stop = fmap (catMaybes . (map buildShift)) $ withPGDB "SELECT S.id, S.user_id, S.place, S.start, S.stop, S.recorded, S.recorder, S.color, S.units FROM shifts AS S WHERE S.place = ? AND S.start > ? AND S.stop < ?;" [toSql $ pId place, toSql start, toSql stop]
  
getUserCurrentShifts :: UserPlace -> User -> Day -> Day -> AppHandler [Shift]
getUserCurrentShifts place user start stop = fmap (catMaybes . (map buildShift)) $ withPGDB "SELECT S.id, S.user_id, S.place, S.start, S.stop, S.recorded, S.recorder, S.color, S.units FROM shifts_current AS S WHERE S.place = ? AND S.user_id = ? AND S.start > ? AND S.stop < ?;" [toSql $ pId place, toSql $ uId user, toSql start, toSql stop]

getUserShift :: BS.ByteString -> BS.ByteString -> AppHandler (Maybe Shift)
getUserShift uid id' = fmap ((>>= buildShift).listToMaybe) $ withPGDB "SELECT S.id, S.user_id, S.place, S.start, S.stop, S.recorded, S.recorder, S.color, S.units FROM shifts_current AS S WHERE S.id = ? AND S.user_id = ?;" [toSql $ id', toSql uid]

getShift :: BS.ByteString -> AppHandler (Maybe Shift)
getShift id' = fmap ((>>= buildShift).listToMaybe) $ withPGDB "SELECT S.id, S.user_id, S.place, S.start, S.stop, S.recorded, S.recorder, S.color, S.units FROM shifts_current AS S WHERE S.id = ?;" [toSql $ id']
  
getShiftChanges :: Shift -> AppHandler [Modification]
getShiftChanges shift = fmap (catMaybes . (map buildChange)) $ withPGDB "SELECT U.name, start, stop, recorder, recorded, color, units FROM shiftchanges JOIN users AS U ON recorder = U.id WHERE old_shift = ?;" [toSql $ sId shift]
  where buildChange (n:srt:stp:rer:red:co:un:[]) = Just $ Change (fromSql srt) (fromSql stp) (colorFromInt (fromSql co)) (fromSql un) (emptyUser {uId = (fromSql rer), uName = (fromSql n)}) (fromSql red)
        buildChange _ = Nothing
  
getShiftDeletes :: Shift -> AppHandler [Modification]
getShiftDeletes shift = fmap (catMaybes . (map buildDelete)) $ withPGDB "SELECT U.name, recorder, recorded FROM shiftdeletes JOIN users AS U ON recorder = U.id WHERE old_shift = ?;" [toSql $ sId shift]
  where buildDelete (name:rer:red:[]) = Just $ Delete (emptyUser {uId = (fromSql rer), uName = (fromSql name)}) (fromSql red)
        buildDelete _ = Nothing
  
getShiftCovers :: Shift -> AppHandler [Modification]
getShiftCovers shift = fmap (catMaybes . (map buildCover)) $ withPGDB "SELECT U.name, coverer, recorded FROM shiftcovers JOIN users AS U ON coverer = U.id WHERE shift_id = ?;"  [toSql $ sId shift]
  where buildCover (n:c:r:[]) = Just $ Cover (emptyUser {uId = (fromSql c), uName = (fromSql n)}) (fromSql r)
        buildCover _ = Nothing
  
