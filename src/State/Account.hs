{-# LANGUAGE OverloadedStrings #-}

module State.Account where


import            Control.Monad
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Maybe (catMaybes, listToMaybe)
import            Database.HDBC
import            Data.Time.LocalTime

import            Snap.Auth

import Application
import State.Types
import State.Place
import qualified Utils as U

getUser uid = do 
  user   <- withPGDB "SELECT id, name, active, super, view FROM users WHERE id = ? LIMIT 1;" [toSql uid]
  places <- getUserPlaces uid
  return $ U.bind2 buildUser (listToMaybe user) (Just places)

updateUserView :: User -> Application Bool
updateUserView user = do
  res <- withPGDB "UPDATE users SET view = ? WHERE id = ? RETURNING id;" [toSql (uView user), toSql (uId user)]
  return $ not (null res)

mkUser au = do (Attrs active super places view) <- liftM snd au
               auth <- liftM fst au
               (UserId id')   <- userId auth
               name <- userEmail auth 
               return $ User id' name active super places view

buildUser (ui:un:ua:us:uv:[]) places = 
  Just (emptyAuthUser { userId = Just $ UserId (fromSql ui) 
                      , userEmail = fromSql un
                      , userPassword = Just (Encrypted "")
                      , userSalt = Just ""
                      }
        , Attrs (fromSql ua) (fromSql us) 
                (map (\(pi:pn:pt:po:pf:[]) -> 
                  UserPlace (fromSql pi) (fromSql pn) (fromSql po) (fromSql pf) (fromSql pt)) 
                places)
                (fromSql uv))
                
buildUser _ _ = Nothing

