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
import qualified  Utils as U

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

checkPlaceLogin (Just org) (Just place) = do u <- currentUser
                                             guard (userHasPlace org place u)
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

loginH loginFailure loginSuccess = do
    euid <- getParams >>= return . EUId 
    password <- getParam "password"
    mMatch <- case password of
      Nothing -> return $ Left PasswordFailure
      Just p  -> performLogin euid
    either loginFailure (const loginSuccess) mMatch

performLogin euid = do
    {-liftIO $ putStrLn $ show euid-}
    getUserExternal euid >>= maybe (return $ Left ExternalIdFailure) login
  where 
    login x@(user, _) = do
      setSessionUserId (userId user) 
      return (Right x)

data User = User { uId :: BS.ByteString
                 , uName :: BS.ByteString
                 , uActive :: Bool
                 , uSuper :: Bool
                 , uPlaces :: [UserPlace]
                 }
              deriving (Eq, Show)
              
data UserPlace = UserPlace { pId    :: Int 
                           , pName  :: BS.ByteString 
                           , pOrg   :: BS.ByteString
                           , pFac   :: Bool 
                           , pToken :: BS.ByteString
                           }
      deriving (Eq, Show)

data Attrs = Attrs Bool Bool [UserPlace]
      deriving (Eq, Show)


currentUser = do au <- currentAuthUser
                 let u = do (Attrs active super places) <- liftM snd au
                            auth <- liftM fst au
                            (UserId id')   <- userId auth
                            name <- userEmail auth 
                            return $ User id' name active super places
                 return u
                 

buildUser (ui:un:ua:us:[]) places = 
  Just (emptyAuthUser { userId = Just $ UserId (fromSql ui) 
                                             , userEmail = fromSql un
                                             , userPassword = Just (Encrypted "")
                                             , userSalt = Just ""
                                             }
        , Attrs (fromSql ua) (fromSql us) 
                (map (\(pi:pn:pt:po:pf:[]) -> 
                  UserPlace (fromSql pi) (fromSql pn) (fromSql po) (fromSql pf) (fromSql pt)) 
                places))
                
buildUser _ _ = Nothing

getUserPlaces :: BS.ByteString -> Application [[SqlValue]]
getUserPlaces uid = withPGDB "SELECT P.id, P.name, P.token, P.organization, PU.facilitator FROM places as P JOIN placeusers AS PU ON PU.place = P.id WHERE PU.user_id = ?;" [toSql uid]

instance MonadAuthUser Application Attrs where
  getUserInternal (UserId uid) = do 
    user   <- withPGDB "SELECT id, name, active, super FROM users WHERE id = ? LIMIT 1;" [toSql uid]
    places <- getUserPlaces uid
    return $ U.bind2 buildUser (listToMaybe user) (Just places)

  getUserExternal (EUId params) = do
    let ps = fmap (map (toSql . B8.concat)) $ sequence [M.lookup "name" params, M.lookup "place" params, M.lookup "password" params]
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
   
    
  