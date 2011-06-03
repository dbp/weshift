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
import            State
import            Mail (mailActivation)


requireUserBounce :: Application () -> Application ()
requireUserBounce good = do
    uri <- liftM rqURI getRequest
    let loginPage = redirect (BS.concat ["/login?redirectTo=", uri])
    requireUser loginPage good

requireUserBounce' :: (User -> Application ()) -> Application ()
requireUserBounce' good = do
    uri <- liftM rqURI getRequest
    let loginPage = redirect (BS.concat ["/login?redirectTo=", uri])
    u <- currentUser
    case u of
      Nothing -> loginPage
      Just user -> good user

checkPlaceLogin = undefined

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

loginH loginFailure loginSuccess = do
    euid <- getParams >>= return . EUId 
    password <- getParam "password"
    mMatch <- case password of
      Nothing -> return $ Left PasswordFailure
      Just p  -> performLogin euid
    either loginFailure (const loginSuccess) mMatch

performLogin euid = do
    liftIO $ putStrLn $ show euid
    getUserExternal euid >>= maybe (return $ Left ExternalIdFailure) login
  where 
    login x@(user, _) = do
      setSessionUserId (userId user) 
      return (Right x)

data User = User { uId :: BS.ByteString
                 , uName :: BS.ByteString
                 , uActive :: Bool
                 , uSuper :: Bool
                 }
              deriving (Eq, Show)

data Attrs = Attrs Bool Bool
      deriving (Eq, Show)


currentUser = do au <- currentAuthUser
                 let u = do (Attrs active super) <- liftM snd au
                            auth <- liftM fst au
                            (UserId id')   <- userId auth
                            name <- userEmail auth 
                            return $ User id' name active super
                 return u
                 

buildUser (i:n:a:s:[]) = Just (emptyAuthUser { userId = Just $ UserId (fromSql i) 
                                             , userEmail = fromSql n
                                             , userPassword = Just (Encrypted "")
                                             , userSalt = Just ""
                                             }
                                , Attrs (fromSql a) (fromSql s))
buildUser _ = Nothing

instance MonadAuthUser Application Attrs where
  getUserInternal (UserId uid) = do 
    resp <- withPGDB $ \c -> quickQuery' c "SELECT id, name, active, super FROM users WHERE id = ? LIMIT 1;" [toSql uid]
    return $ buildUser =<< listToMaybe resp

  getUserExternal (EUId params) = do
    let ps = fmap (map (toSql . B8.concat)) $ sequence [M.lookup "name" params, M.lookup "place" params, M.lookup "password" params]
    resp <- maybe (return []) 
                  (\p -> withPGDB (\c -> quickQuery' c
                  "SELECT id, name, active, super FROM users JOIN placeusers ON placeusers.user_id = users.id WHERE users.name = ? AND placeusers.place = ? AND password = crypt(?, password) LIMIT 1;" p)) 
                  ps
    liftIO $ putStrLn "Logging in user"
    liftIO $ putStrLn $ show $ buildUser =<< listToMaybe resp
    return $ buildUser =<< listToMaybe resp
                 
  
  -- no remember tokens for now.
  getUserByRememberToken _ = return Nothing
  -- we are going to handle saving / creating users ourselves, as it isn't as simple as "save user".
  saveAuthUser _ = return Nothing
   
    
  