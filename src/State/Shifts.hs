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

getNextShift :: User -> UserPlace -> Application (Maybe Shift)
getNextShift u p =
  fmap ((>>= buildShift).listToMaybe) $ withPGDB "SELECT id, user_id, place, start, stop, recorded, recorder FROM shifts_current WHERE user_id = ? AND place = ? AND start > now() ORDER BY start ASC LIMIT 1;" [toSql $ uId u, toSql $ pId p]


getShifts :: Day -> Day -> UserPlace -> Application [Shift]
getShifts start end p = fmap (catMaybes . (map buildShift)) $ withPGDB "SELECT id, user_id, place, start, stop, recorded, recorder FROM shifts_current WHERE place = ? AND start >= ? AND start < ? ORDER BY start;" [toSql $ pId p, toSql start, toSql end]

getUncoveredShifts :: Day -> Day -> UserPlace -> Application [Shift]
getUncoveredShifts start end p = fmap (catMaybes . (map buildShift)) $ withPGDB "SELECT C.id, C.user_id, C.place, C.start, C.stop, C.recorded, C.recorder FROM shiftrequests AS R JOIN shifts_current as C ON shift_id = C.id WHERE C.start > now() AND C.start >= ? AND C.start < ? AND C.place = ?;" [toSql start, toSql end, toSql $ pId p]