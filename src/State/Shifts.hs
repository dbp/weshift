{-# LANGUAGE OverloadedStrings #-}

module State.Shifts where


import            Control.Monad
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Maybe (catMaybes, listToMaybe)
import            Database.HDBC
import            Data.Time.LocalTime

import Application
import State.Types

getNextShift :: User -> UserPlace -> Application (Maybe Shift)
getNextShift (User uid _ _ _ _) (UserPlace pid _ _ _ _) = do
  fmap ((>>= buildShift).listToMaybe) $ withPGDB "SELECT * FROM shifts WHERE user_id = ? AND place = ? AND start > now() ORDER BY start ASC LIMIT 1;" [toSql uid, toSql pid]

