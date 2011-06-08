{-# LANGUAGE OverloadedStrings #-}

module State.Coworkers where


import            Control.Monad
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Maybe (catMaybes, listToMaybe)
import            Database.HDBC
import            Data.Time.LocalTime

import Application
import State.Types
import Auth

getCoworkers :: User -> UserPlace -> Application [User]
getCoworkers u p = do
  users <- withPGDB "SELECT PU.facilitator, U.id, U.name, U.active, U.super FROM users AS U JOIN placeusers AS PU ON PU.user_id = U.id WHERE PU.place = ? ORDER BY name ASC;" [toSql $ pId p]
  return $ filter (\c -> uId c /= uId u) $ catMaybes $ map mkUser $ map (\su -> buildUser (tail su) [[toSql (""::String), toSql (""::String), toSql (""::String), toSql (""::String), head su]]) users
