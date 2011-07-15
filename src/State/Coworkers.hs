{-# LANGUAGE OverloadedStrings #-}

module State.Coworkers where


import            Control.Monad
import            Control.Monad.Trans
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Maybe (catMaybes, mapMaybe, listToMaybe)
import            Database.HDBC
import            Data.Time.LocalTime

import Application
import State.Types
import State.Account
import State.Place
import Auth

getCoworkers :: User -> UserPlace -> Application [User]
getCoworkers u p = do us <- getWorkers p
                      return $ filter (\c -> uId c /= uId u) us

getWorkers :: UserPlace -> Application [User]
getWorkers p = do
  -- don't send back users who have name "" - that means they have been disabled
  users <- withPGDB "SELECT PU.facilitator, U.id, U.name, U.active, U.super, U.view FROM users AS U JOIN placeusers AS PU ON PU.user_id = U.id WHERE PU.place = ? AND U.name != '' ORDER BY name ASC;" [toSql $ pId p]
  return $ catMaybes $ map mkUser $ map (\su -> buildUser (tail su) [[toSql (""::String), toSql (""::String), toSql (""::String), toSql (""::String), head su]]) users

getUsersByName :: BS.ByteString -> Application [User]
getUsersByName n = do
  users <- withPGDB "SELECT id, name, active, super, view FROM users WHERE name = ? ORDER BY name ASC;" [toSql n]
  users' <- mapM buildUS users
  {-liftIO $ putStrLn $ show users
  liftIO $ putStrLn $ show users'-}
  return $ catMaybes users'
    where buildUS (i:n:a:s:v:[]) = do places <- fmap (mapMaybe buildPl) $ getUserPlaces (fromSql i)
                                      return $ Just $ (User (fromSql i) (fromSql n) (fromSql a) (fromSql s) places (fromSql v))
          buildUS _ = return Nothing
          buildPl (pi:pn:pt:po:pf:[]) = Just $ UserPlace (fromSql pi) (fromSql pn) (fromSql po) (fromSql pf) (fromSql pt) 
          buildPl _ = Nothing