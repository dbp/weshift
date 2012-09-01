{-# LANGUAGE OverloadedStrings #-}

module Handlers.Coworkers where

import Data.Maybe (fromJust, isNothing)  
import Control.Monad (when, liftM)
import Control.Monad.Trans

import Text.Templating.Heist
import Snap.Snaplet.Heist
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T  
import qualified Data.ByteString.Char8 as B8  
import qualified Data.ByteString as BS  
import Snap.Types

import Text.Digestive
import Text.Digestive.Heist
import Text.Digestive.Snap

import State.Types
import State.Coworkers
import State.Account
import Application
import Common
import Auth
import Mail
import Handlers.Settings (nameForm)
import Splices.Place

-- | important: the place that is passed is the place of the logged in user, so it's "pFac" will not be relevant.
renderCoworker :: User -> Splice AppHandler
renderCoworker (User uid uname uact usuper uplaces uview utoken) = do
  let place = head uplaces
  runChildrenWith [("id",         textSplice $ TE.decodeUtf8 uid)
                  ,("name",       textSplice $ TE.decodeUtf8 uname)
                  ,("classes",    textSplice $ T.concat (["member"] ++ if pFac place then [" facilitator"] else []))
                  ,("places",     renderPlaces Nothing uplaces)
                  ,("fac",        if pFac place then identitySplice else blackHoleSplice)
                  ,("notfac",     if pFac place then blackHoleSplice else identitySplice)
                  ]
                       
renderCoworkers :: [User] -> Splice AppHandler
renderCoworkers coworkers = mapSplices renderCoworker coworkers


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

userNotIn place = checkM "Another user with this name already exists at this place." $ \n -> fmap not $ userExists place n

newUserForm p = userNotIn p nameForm

addCW u p = do  
  (view, result) <- runForm "add-user-form" (newUserForm p)
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
                             (view, result) <- runForm "optional-email-form" emailOrBlankForm
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

--validEmailOrBlank :: Validator AppHandler T.Text String
validEmailOrBlank = check "Must be a valid email, like help@weshift.org" $ \e -> let s = TE.encodeUtf8 e in ('@' `B8.elem` s && '.' `B8.elem` s) || (T.null e)

--emailOrBlankForm :: SnapForm AppHandler T.Text HeistView String
emailOrBlankForm = "email" .: validEmailOrBlank (text Nothing)
  

coworkersSplice cs = [("coworkersCount", textSplice $ T.pack $ show $ length cs)
                     ,("coworkers", renderCoworkers cs)
                     ,("name-errors", blackHoleSplice)
                     ]