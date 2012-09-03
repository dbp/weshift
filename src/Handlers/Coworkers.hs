{-# LANGUAGE OverloadedStrings #-}

module Handlers.Coworkers where

-- | Boilerplate imports
import            Imports
import qualified  Data.Text as T
import qualified  Data.Text.Encoding as TE
import qualified  Data.Bson as B
import qualified  Data.Map as M
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import qualified  Text.XmlHtml as X
import qualified  Utils as U

-- | Module specific imports
import State.Coworkers
import State.Account
import Mail
import Render.Coworkers
import Forms.Coworkers

coworkersH :: User -> UserPlace -> AppHandler ()
coworkersH user place = 
  route [("/", ifTop $ listCoworkers user place)
        ,("/add", if pFac place then addCoworkerH user place else pass)
        ,("/facilitate", facilitateH user place)
        ,("/delete", deleteH user place)
        ]

-- | This allows a current facilitator to make another person into one.
facilitateH u p = do
  muid <- getParam "id"
  case muid of
    Nothing -> listCoworkers u p
    Just uid -> do
      if pFac p then setFacilitator uid p True else return False
      listCoworkers u p
      
-- | This allows a current facilitator to remove a user from the place.
deleteH u p = do
  muid <- getParam "id"
  case muid of
    Nothing -> listCoworkers u p
    Just uid -> do
      if pFac p then removeUser uid p else return False
      listCoworkers u p

  
listCoworkers user place = do 
  coworkers <- getCoworkers user place  -- Note: this gives back incomplete place lists, just the current place
  setView user "profile" "profile.coworkers"
  heistLocal (bindSplices (coworkersSplice coworkers)) $ renderWS "profile/coworkers/default"

addCoworkerH u p = do
  route [("/", ifTop $ addCW u p)
        ,("/exists", addCWexists u p)
        ,("/new", addCWnew u p)
        ]

addCW u p = do  
  (view, result) <- wsForm (newUserForm p)
  case result of
    Nothing -> do
      heistLocal (bindDigestiveSplices view) $ renderWS "profile/coworkers/add"
    Just name' -> do
      us <- getUsersByName (TE.encodeUtf8 name')
      case us of
        [] -> heistLocal (bindSplices [("name", textSplice name')]) $ renderWS "profile/coworkers/add_new"
        _ -> heistLocal (bindSplices [("name", textSplice name'),("users", renderCoworkers us)]) $ renderWS "profile/coworkers/add_existing"

addCWexists u p = do
  i <- getParam "id"
  case i of
    Nothing -> pass
    Just id' -> do
      addUserPlace p i
      listCoworkers u p
  
addCWnew u p = do
  n <- getParam "name"
  server  <- liftM rqServerName getRequest
  portNum <- liftM rqServerPort getRequest
  case n of
    Just name -> do
      ti <- newUser name p
      case ti of
        Just (token,i) -> do addUserPlace p i
                             let actLink = BS.concat ["http://"
                                                     , server
                                                     , if portNum /= 80 then (B8.pack $ ':' : (show portNum)) else ""
                                                     , "/activate/account?id="
                                                     , i
                                                     , "&token="
                                                     , token
                                                     , "&pl="
                                                     , (pId p)
                                                     ]
                             (view, result) <- wsForm emailOrBlankForm
                             case result of
                                 Nothing -> do
                                   heistLocal (bindDigestiveSplices view) $ renderWS "profile/coworkers/add_new"
                                 Just email' -> 
                                  case TE.encodeUtf8 email' of
                                    "" -> heistLocal (bindSplices [("link", textSplice (TE.decodeUtf8 actLink))]) $ renderWS "profile/coworkers/add_success"
                                    email -> do mailAccountActivation name email (BS.concat [actLink,"&em=",urlEncode email])
                                                listCoworkers u p
        _ -> renderWS "profile/coworkers/add_error" -- the only thing that should have been able to go wrong 
                                                  -- should have been checked earlier, so we don't know what happened
    Nothing -> pass -- shouldn't be able to get here, so pass
  
