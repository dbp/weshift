{-# LANGUAGE OverloadedStrings #-}

module Handlers.Coworkers where

import Data.Maybe (fromJust, isNothing)  
import Control.Monad (when, liftM)
import Control.Monad.Trans

import Text.Templating.Heist
import Snap.Extension.Heist
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T  
import qualified Data.ByteString.Char8 as B8  
import qualified Data.ByteString as BS  
import Snap.Types

import Text.Digestive.Types
import Text.Digestive.Snap.Heist
import Text.Digestive.Validate

import State.Types
import State.Coworkers
import State.Account
import Application
import Common
import Auth
import Mail
import Handlers.Settings (nameForm)
import Splices.Place

renderCoworker :: User -> Splice Application
renderCoworker (User uid uname uact usuper uplaces uview) = do -- utoken
  runChildrenWith [("id",         textSplice $ TE.decodeUtf8 uid)
                  ,("name",       textSplice $ TE.decodeUtf8 uname)
                  ,("classes",    textSplice $ T.concat (["member"] ++ if pFac (head uplaces) then [" facilitator"] else []))
                  ,("places",     renderPlaces uplaces)
                  ]
                       
renderCoworkers :: [User] -> Splice Application
renderCoworkers coworkers = mapSplices renderCoworker coworkers


coworkersH :: User -> UserPlace -> Application ()
coworkersH user place = 
  route [("/", ifTop $ listCoworkers user place)
        ,("/add", if pFac place then addCoworkerH user place else pass)
        ]
  
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

newUserForm p = nameForm `validate` (userNotIn p)

addCW u p = do  
  r <- eitherSnapForm (newUserForm p) "add-user-form"
  case r of
    Left splices' -> do
      heistLocal (bindSplices splices') $ renderWS "profile/coworkers/add"
    Right name' -> do
      us <- getUsersByName (B8.pack name')
      case us of
        [] -> heistLocal (bindSplices [("name", textSplice $ T.pack name')]) $ renderWS "profile/coworkers/add_new"
        _ -> heistLocal (bindSplices [("name", textSplice $ T.pack name'),("users", renderCoworkers us)]) $ renderWS "profile/coworkers/add_existing"

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
                             r <- eitherSnapForm emailOrBlankForm "optional-email-form"
                             case r of
                                 Left splices' -> do
                                   heistLocal (bindSplices splices') $ renderWS "profile/coworkers/add_new"
                                 Right email' -> 
                                  case B8.pack email' of
                                    "" -> heistLocal (bindSplices [("link", textSplice (TE.decodeUtf8 actLink))]) $ renderWS "profile/coworkers/add_success"
                                    email -> do mailAccountActivation name email (BS.concat [actLink,"&em=",urlEncode email])
                                                listCoworkers u p
        _ -> renderWS "profile/coworkers/add_error" -- the only thing that should have been able to go wrong 
                                                  -- should have been checked earlier, so we don't know what happened
    Nothing -> pass -- shouldn't be able to get here, so pass

validEmailOrBlank :: Validator Application T.Text String
validEmailOrBlank = check "Must be a valid email, like help@weshift.org" $ \e -> ('@' `elem` e && '.' `elem` e) || (null e)

emailOrBlankForm :: SnapForm Application T.Text HeistView String
emailOrBlankForm = input "email" Nothing  `validate` validEmailOrBlank <++ errors 
  

coworkersSplice cs = [("coworkersCount", textSplice $ T.pack $ show $ length cs)
                     ,("coworkers", renderCoworkers cs)
                     ,("name-errors", blackHoleSplice)
                     ]