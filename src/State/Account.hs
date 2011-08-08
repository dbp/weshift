{-# LANGUAGE OverloadedStrings #-}

module State.Account where


import            Control.Monad
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Maybe (catMaybes, mapMaybe, listToMaybe)
import            Database.HDBC
import            Data.Time.LocalTime

import            Snap.Auth

import Application
import State.Types
import State.Place
import qualified Utils as U

getUser uid = do 
  user   <- withPGDB "SELECT id, name, active, super, view FROM users WHERE id = ? LIMIT 1;" [toSql uid] -- ,token
  places <- getUserPlaces uid
  return $ U.bind2 buildUser (listToMaybe user) (Just places)

getUserEmails u =
  fmap (mapMaybe buildEmail) $ withPGDB "SELECT id,user_id,email,confirmed FROM useremails WHERE user_id = ?;" [toSql (uId u)]

newUser n p =
  fmap ((>>= mkRet) . listToMaybe) $ withPGDB "INSERT INTO users (SELECT nextval('users_id_seq'), ? AS name, md5(random()::text) AS password WHERE NOT EXISTS (SELECT P.id FROM places AS P JOIN placeusers AS PU ON PU.place = P.id JOIN users AS U ON PU.user_id = U.id WHERE U.name = ? AND P.id = ?) AND EXISTS (SELECT id FROM places WHERE id = ?)) RETURNING id, password;" [toSql n, toSql n, toSql (pId p), toSql (pId p)]
  -- , substring(md5(random()::text) from 0 for 10) AS token
    where mkRet (i:t:[]) = Just (fromSql t, fromSql i)
          mkRet _ = Nothing

addUserPlace p i = fmap (not.null) $ withPGDB "INSERT INTO placeusers (user_id, place) VALUES (?, ?) RETURNING user_id;" [toSql i, toSql (pId p)]

addFacilitatorPlace p i = fmap (not.null) $ withPGDB "INSERT INTO placeusers (user_id, place, facilitator) VALUES (?, ?, true) RETURNING user_id;" [toSql i, toSql (pId p)]

userExists p n = 
  fmap (not.null) $ withPGDB "SELECT P.id FROM places AS P JOIN placeusers AS PU ON PU.place = P.id JOIN users AS U ON PU.user_id = U.id WHERE U.name = ? AND P.id = ?" [toSql n, toSql (pId p)]

setUserPassword u pw = 
  fmap (not.null) $ withPGDB "UPDATE users SET password = crypt(?, gen_salt('bf')) WHERE id = ? RETURNING id;" [toSql pw, toSql (uId u)]
  
addUserEmail u e = 
  fmap ((fmap fromSql) . (>>= listToMaybe) . listToMaybe) $ withPGDB "INSERT INTO useremails (user_id, email) VALUES (?, ?) RETURNING token;" [toSql (uId u), toSql e]

addUserEmail' u e =
  fmap (not.null) $ withPGDB "INSERT INTO useremails (user_id, email, confirmed) VALUES (?, ?, true) RETURNING token;" [toSql (uId u), toSql e]

confirmUserEmail u t =
  fmap (not.null) $ withPGDB "UPDATE useremails SET confirmed = true WHERE token = ? AND user_id = ? RETURNING id;" [toSql t, toSql u]


deleteUserEmail u e = 
  fmap (not.null) $ withPGDB "DELETE FROM useremails WHERE id = ? AND user_id = ? RETURNING id;" [toSql e, toSql (uId u)]

-- | this erases all personally identifiable info (ie, emails and their name) and replaces their password with an activation token
-- so that they can re-activate it later if they want. They must have an active email acount (which will be deleted in the process).
disableAccount u = do
  em <- fmap ((fmap fromSql) . (>>= listToMaybe) . listToMaybe) $ withPGDB "SELECT email FROM useremails WHERE user_id = ? AND confirmed = true;" [toSql (uId u)]
  case em of
    Nothing -> return Nothing -- if they have NO emails, we can't disable, because we wouldn't be able to send them a message
    Just email -> do
      token <- fmap ((fmap fromSql) . (>>= listToMaybe) . listToMaybe) $ withPGDB "UPDATE users SET name = '', password = substring(md5((random())::text), 1, 10), active = false WHERE id = ? RETURNING password;" [toSql (uId u)]
      case token of 
        Nothing -> return Nothing
        Just t -> do withPGDB "DELETE FROM useremails WHERE user_id = ?;" [toSql (uId u)]
                     return $ Just (email, t)
  
-- | we set their password, restore their name, and set it to open to their email settings so they can,
-- if they want, set their email.
enableAccount u n pw = 
  fmap (not.null) $ withPGDB "UPDATE users SET password = crypt(?, gen_salt('bf')), name = ?, view = 'work.month;profile.settings.email;messages' WHERE id = ? RETURNING id;" [toSql pw, toSql n, toSql (uId u)]

activateAccount u p =
  fmap (not.null) $ withPGDB "UPDATE users SET password = crypt(?, gen_salt('bf')), active = true WHERE id = ? RETURNING id;" [toSql p, toSql (uId u)]
  
  

buildEmail (i:u:e:c:[]) = Just (Email (fromSql i) (fromSql u) (fromSql e) (fromSql c))
buildEmail _ = Nothing

updateUserView :: User -> Application Bool
updateUserView user = do
  res <- withPGDB "UPDATE users SET view = ? WHERE id = ? RETURNING id;" [toSql (uView user), toSql (uId user)]
  return $ not (null res)

mkUser au = do (Attrs active super places view) <- liftM snd au -- token
               auth <- liftM fst au
               (UserId id')   <- userId auth
               name <- userEmail auth
               return $ User id' name active super places view -- token

buildUser (ui:un:ua:us:uv:[]) places =  -- :ut
  Just (emptyAuthUser { userId = Just $ UserId (fromSql ui) 
                      , userEmail = fromSql un
                      , userPassword = Just (Encrypted "")
                      , userSalt = Just ""
                      }
        , Attrs (fromSql ua) (fromSql us) 
                (map (\(pi:pn:pt:po:pf:[]) -> 
                  UserPlace (fromSql pi) (fromSql pn) (fromSql po) (fromSql pf) (fromSql pt)) 
                places)
                (fromSql uv)
                {-(fromSql ut)-})
                
buildUser _ _ = Nothing

