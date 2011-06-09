{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Auth where

import            Snap.Auth hiding (performLogin) -- using our own
import            Snap.Auth.Handlers

import            Snap.Extension.Session.CookieSession
import qualified  Data.Map as M
import            Control.Monad
import            Control.Monad.Reader
import            Control.Monad.Trans
import            Control.Applicative
import            Snap.Types
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8

import            Text.Templating.Heist
import            Text.XmlHtml (childNodes)

import            Snap.Extension.Heist
import            Data.Maybe (fromMaybe, fromJust, isJust, listToMaybe, catMaybes)
import qualified  Data.Bson as B
import            System.Random

import            Database.HDBC

import            Application
import            State.Types
import            State.Place
import            State.Account
import            Mail (mailActivation)
import qualified  Utils as U

repSpaces = B8.map (\c -> if c == '_' then ' ' else c)
repUnders = B8.map (\c -> if c == ' ' then '_' else c)


requireUserBounce :: Application () -> Application ()
requireUserBounce good = do
    uri <- liftM rqURI getRequest
    let loginPage = redirect (BS.concat ["/login?redirectTo=", uri])
    requireUser loginPage good

requireUserBounce' :: (User -> Application ()) -> Application ()
requireUserBounce' good = do
    uri <- liftM rqURI getRequest
    let loginPage = redirect (BS.concat ["/login?redirectTo=", uri])
    u <- getCurrentUser
    case u of
      Nothing -> loginPage
      Just user -> good user

checkPlaceLogin (Just org) (Just place) = do u <- getCurrentUser
                                             uri <- liftM rqURI getRequest
                                             let loginPage = do p <- getPlace (repSpaces org) (repSpaces place)
                                                                let pid = maybe "" pId p
                                                                redirect (BS.concat ["/login?redirectTo=", uri, "&pl=", pid])
                                             unless (userHasPlace org place u) loginPage
  where userHasPlace org place (Just (User _ _ _ super places)) = super || any (\p -> pName p == place && pOrg p == org) places
        userHasPlace _ _ Nothing = False
checkPlaceLogin _ _ = mzero
        
--- the following two taken from https://github.com/mightybyte/snap-heist-splices which depends on unreleased version of snap
------------------------------------------------------------------------------
-- | Renders the child nodes only if the request comes from an authenticated
-- user.
ifLoggedIn :: (MonadAuth m, MonadAuthUser m Attrs) => Splice m
ifLoggedIn = do
    node <- getParamNode
    res <- lift $ requireUser (return []) (return $ childNodes node)
    return res

------------------------------------------------------------------------------
-- | Renders the child nodes only if the request comes from a user that is not
-- logged in.
ifGuest :: (MonadAuth m, MonadAuthUser m Attrs) => Splice m
ifGuest = do
    node <- getParamNode
    res <- lift $ requireUser (return $ childNodes node) (return [])
    return res

performLogin euid = do
    {-liftIO $ putStrLn $ show euid-}
    getUserExternal (EUId euid) >>= maybe (return $ Left ExternalIdFailure) login
  where 
    login x@(user, _) = do
      setSessionUserId (userId user) 
      maybe (return ()) (setInSession "place" . BS.concat) $ M.lookup "pl" euid
      return (Right x)

getCurrentUser = do au <- currentAuthUser
                    let u = mkUser au
                    return u
                 

instance MonadAuthUser Application Attrs where
  getUserInternal (UserId uid) = getUser uid
  
  getUserExternal (EUId params) = do
    let ps = fmap (map (toSql . B8.concat)) $ sequence [M.lookup "name" params, M.lookup "pl" params, M.lookup "password" params]
    resp <- maybe (return []) 
                  (withPGDB "SELECT id, name, active, super FROM users JOIN placeusers ON placeusers.user_id = users.id WHERE users.name = ? AND placeusers.place = ? AND password = crypt(?, password) LIMIT 1;") 
                  ps
    places <- maybe (return []) (getUserPlaces . fromSql . head) (listToMaybe resp)
    {-liftIO $ putStrLn "Logging in user"
    liftIO $ putStrLn $ show $ buildUser =<< listToMaybe resp-}
    return $ U.bind2 buildUser (listToMaybe resp) (Just places)
                 
  
  -- no remember tokens for now.
  getUserByRememberToken _ = return Nothing
  -- we are going to handle saving / creating users ourselves, as it isn't as simple as "save user".
  saveAuthUser _ = return Nothing
   
    
  