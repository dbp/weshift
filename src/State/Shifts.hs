{-# LANGUAGE OverloadedStrings #-}

module State.Shifts where


import            Control.Monad
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Maybe (catMaybes, listToMaybe)
import            Database.HDBC
import            Data.Time.LocalTime
import            Data.Time.Calendar

import Application
import State.Types

data Modification = Delete User LocalTime -- who deleted it, and when
                  | Change LocalTime LocalTime User LocalTime -- new start, new end, who did it, when it was changed
                  | Cover User LocalTime -- who covered it, and when
                  deriving (Eq, Show)
mTime (Delete _ t) = t
mTime (Change _ _ _ t) = t
mTime (Cover _ t) = t

-- | if successful, returns Right id, else Left shift that could not be entered
insertShift :: Shift -> Application (Either Shift BS.ByteString)
insertShift s@(Shift _ u p start stop _ recorder) = 
  fmap (\r -> if not (null r) && not (null $ head r) then Right (fromSql $ head $ head r) else Left s) $ withPGDB "INSERT INTO shifts (place, user_id, start, stop, recorder) (SELECT ? as place, ? as user_id, ? as start, ? as stop, ? as recorder WHERE NOT EXISTS (SELECT id, user_id, start, stop FROM shifts_current WHERE user_id = ? AND (? < stop AND ? > start) UNION ALL select id, user_id, start, stop FROM obligations WHERE user_id = ? AND (? < stop AND ? > start))) RETURNING id;" [toSql p,toSql u, toSql start, toSql stop, toSql recorder, toSql u, toSql start, toSql stop, toSql u, toSql start, toSql stop]

-- | note: this does not actually delete a shift, it merely marks it as deleted.
deleteShift :: User -> Shift -> Application Bool
deleteShift u s = fmap (not.null) $ withPGDB "INSERT INTO shiftdeletes (place, user_id, old_shift, recorder) VALUES (?, ?, ?, ?) RETURNING old_shift;" [toSql (sPlace s), toSql (sUser s), toSql (sId s), toSql (uId u)]


checkShiftTime :: BS.ByteString -> LocalTime -> LocalTime -> Application Bool
checkShiftTime uid start stop = fmap null $ withPGDB "SELECT id FROM shifts_current WHERE user_id = ? AND (? < stop AND ? > start)" [toSql uid, toSql start, toSql stop]

getNextShift :: User -> UserPlace -> Application (Maybe Shift)
getNextShift u p =
  fmap ((>>= buildShift).listToMaybe) $ withPGDB "SELECT id, user_id, place, start, stop, recorded, recorder FROM shifts_current WHERE user_id = ? AND place = ? AND start > now() ORDER BY start ASC LIMIT 1;" [toSql $ uId u, toSql $ pId p]


getShifts :: Day -> Day -> UserPlace -> Application [Shift]
getShifts start end p = fmap (catMaybes . (map buildShift)) $ withPGDB "SELECT id, user_id, place, start, stop, recorded, recorder FROM shifts_current WHERE place = ? AND start >= ? AND start < ? ORDER BY start;" [toSql $ pId p, toSql start, toSql end]

getUncoveredShifts :: Day -> Day -> UserPlace -> Application [Shift]
getUncoveredShifts start end p = fmap (catMaybes . (map buildShift)) $ withPGDB "SELECT C.id, C.user_id, C.place, C.start, C.stop, C.recorded, C.recorder FROM shiftrequests AS R JOIN shifts_current as C ON shift_id = C.id WHERE C.start > now() AND C.start >= ? AND C.start < ? AND C.place = ?;" [toSql start, toSql end, toSql $ pId p]



getOriginalShifts :: UserPlace -> Day -> Day -> Application [Shift]
getOriginalShifts place start stop = fmap (catMaybes . (map buildShift)) $ withPGDB "SELECT S.id, S.user_id, S.place, S.start, S.stop, S.recorded, S.recorder FROM shifts AS S WHERE S.place = ? AND S.start > ? AND S.stop < ?;" [toSql $ pId place, toSql start, toSql stop]
  
getUserCurrentShifts :: UserPlace -> User -> Day -> Day -> Application [Shift]
getUserCurrentShifts place user start stop = fmap (catMaybes . (map buildShift)) $ withPGDB "SELECT S.id, S.user_id, S.place, S.start, S.stop, S.recorded, S.recorder FROM shifts_current AS S WHERE S.place = ? AND S.user_id = ? AND S.start > ? AND S.stop < ?;" [toSql $ pId place, toSql $ uId user, toSql start, toSql stop]

getUserShift :: User -> BS.ByteString -> Application (Maybe Shift)
getUserShift user id' = fmap ((>>= buildShift).listToMaybe) $ withPGDB "SELECT S.id, S.user_id, S.place, S.start, S.stop, S.recorded, S.recorder FROM shifts_current AS S WHERE S.id = ? AND S.user_id = ?;" [toSql $ id', toSql $ uId user]

  
getShiftChanges :: Shift -> Application [Modification]
getShiftChanges shift = fmap (catMaybes . (map buildChange)) $ withPGDB "SELECT U.name, start, stop, recorder, recorded FROM shiftchanges JOIN users AS U ON recorder = U.id WHERE old_shift = ?;" [toSql $ sId shift]
  where buildChange (n:srt:stp:rer:red:[]) = Just $ Change (fromSql srt) (fromSql stp) (emptyUser {uId = (fromSql rer), uName = (fromSql n)}) (fromSql red)
        buildChange _ = Nothing
  
getShiftDeletes :: Shift -> Application [Modification]
getShiftDeletes shift = fmap (catMaybes . (map buildDelete)) $ withPGDB "SELECT U.name, recorder, recorded FROM shiftdeletes JOIN users AS U ON recorder = U.id WHERE old_shift = ?;" [toSql $ sId shift]
  where buildDelete (name:rer:red:[]) = Just $ Delete (emptyUser {uId = (fromSql rer), uName = (fromSql name)}) (fromSql red)
        buildDelete _ = Nothing
  
getShiftCovers :: Shift -> Application [Modification]
getShiftCovers shift = fmap (catMaybes . (map buildCover)) $ withPGDB "SELECT U.name, coverer, recorded, U.name FROM shiftcovers JOIN users AS U ON coverer = U.id WHERE shift_id = ?;"  [toSql $ sId shift]
  where buildCover (n:c:r:[]) = Just $ Cover (emptyUser {uId = (fromSql c), uName = (fromSql n)}) (fromSql r)
        buildCover _ = Nothing
  