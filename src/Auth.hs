{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Auth where

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as B8
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE

import           Heist
import           Text.XmlHtml                (childNodes)

import qualified Data.Bson                   as B
import qualified Data.Map                    as M
import           Data.Maybe                  (catMaybes, fromJust, fromMaybe,
                                              isJust, listToMaybe)
import           System.Random

import           Database.HDBC
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Common

import           Control.Monad
import           Control.Monad.Trans         (liftIO)

import           Application
import           State.Account
import           State.Place
import           State.Types
import qualified Utils                       as U

repSpaces = T.map (\c -> if c == '_' then ' ' else c)
repUnders = T.map (\c -> if c == ' ' then '_' else c)


requireUserBounce :: AppHandler () -> AppHandler ()
requireUserBounce good = requireUserBounce' (const good)

requireUserBounce' :: (User -> AppHandler ()) -> AppHandler ()
requireUserBounce' good = do
    uri <- liftM rqURI getRequest
    let loginPage = redirect (BS.concat ["/login?redirectTo=", uri])
    u <- getCurrentUser
    case u of
      Nothing -> loginPage
      Just user -> good user

-- NOTE TO ANYONE READING THIS:
-- We had a login system for a legacy application written in OCaml that offloads much of
-- the processing to Postgres - this made it difficult and unecessary to use snaps auth
-- infrastructure, so we are just hacking in some glue to make the useful parts, like
-- session handling, work with this system. Some code here comes right from auth internals.

------------------------------------------------------------------------------
-- | Set the current user's 'UserId' in the active session
--
setSessionUserId :: T.Text -> AppHandler ()
setSessionUserId t = do
  with sess $ setInSession "__user_id" t
  with sess commitSession

------------------------------------------------------------------------------
-- | Remove 'UserId' from active session, effectively logging the user out.
removeSessionUserId :: AppHandler ()
removeSessionUserId = do
  with sess $ deleteFromSession "__user_id"
  with sess commitSession

------------------------------------------------------------------------------
-- | Get the current user's 'UserId' from the active session
--
getSessionUserId :: AppHandler (Maybe T.Text)
getSessionUserId = with sess $ getFromSession "__user_id"

authenticatedUserId = do
  muid <- getSessionUserId
  u <- maybe (return Nothing) getUser muid
  return (fmap uId u)

performLogin :: Params -> AppHandler (Either String User)
performLogin params = do
    getUserFromCreds params >>= maybe (return $ Left "Auth Failure") login
  where
    getUserFromCreds params = do
      let ps = fmap (map (toSql . B8.concat)) $ sequence [M.lookup "name" params, M.lookup "pl" params, M.lookup "password" params]
      resp <- maybe (return [])
                    (withPGDB "SELECT id, name, active, super, view, token FROM users JOIN placeusers ON placeusers.user_id = users.id WHERE users.name = ? AND placeusers.place = ? AND password = crypt(?, password) LIMIT 1;")
                    ps
      places <- maybe (return []) (getUserPlaces . fromSql . head) (listToMaybe resp)
      return $ U.bind2 buildUser (listToMaybe resp) (Just places)
    login user = do
      setSessionUserId (uId user)
      maybe (return ()) (\p -> with sess $ ((setInSession "place") . TE.decodeUtf8 . BS.concat) p) $ M.lookup "pl" params
      return (Right user)

wsPerformLogout :: AppHandler ()
wsPerformLogout = do
  with sess $ deleteFromSession "place"
  removeSessionUserId

getCurrentUser :: AppHandler (Maybe User)
getCurrentUser = do
  muid <- getSessionUserId
  case muid of
    Nothing -> return Nothing
    Just uid -> getUser uid
