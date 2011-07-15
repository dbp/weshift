{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Handlers.Account where

import Data.Maybe (isNothing, fromJust, listToMaybe)  
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as TE
import Data.Text (Text)

import Snap.Types
import Common
import Application
import Auth
import State.Types
import State.Place
import State.Account
import Handlers.Settings
import Text.Digestive.Snap.Heist
import Text.Digestive.Types
import Text.Digestive.Validate

import Text.Templating.Heist
import Snap.Extension.Heist
import Control.Monad
import "mtl" Control.Monad.Trans
import Control.Applicative

import qualified Snap.Auth as A


loginGetH _ = do pl <- getParam "pl" -- this is the place
                 place <- maybe (return Nothing) getPlaceFromId pl
                 when (isNothing place) $ redirect "/"
                 rq <- getRequest
                 heistLocal (bindStrings [("placeName", TE.decodeUtf8 $ placeName (fromJust place))
                                         ,("placeRoot", TE.decodeUtf8 $ placeRoot (fromJust place))
                                         ,("placeId", TE.decodeUtf8 $ fromJust pl)
                                         ,("url", TE.decodeUtf8 $ rqURI rq)]) $ renderWS "login"
                                    

loginPostH loginFailure loginSuccess = do
    euid <- getParams
    password <- getParam "password"
    mMatch <- case password of
      Nothing -> return $ Left A.PasswordFailure
      Just p  -> performLogin euid
    either loginFailure (const loginSuccess) mMatch

signupH = do
  u <- getCurrentUser
  r <- eitherSnapForm (signupForm u) "signup-form"
  case r of
      Left splices' -> do
        heistLocal (bindSplices splices') $ renderWS "signup_form"
      Right (SignupCreds org place name) -> do
        createOrganizationIfNotExists (B8.pack org)
        mpid <- createPlace (B8.pack org) (B8.pack place)
        case mpid of
          Nothing -> renderWS "signup_error"
          Just pid -> do
            let newPlace = (emptyUserPlace {pId = pid, pName = (B8.pack place), pOrg = (B8.pack org), pFac = True})
            case u of
              Just user -> do -- an existing user
                addFacilitatorPlace newPlace (uId user)
                redirectAsync (placeRoot newPlace)
              Nothing -> do -- this is a new user
                mit <- newUser (B8.pack name) newPlace
                case mit of
                  Nothing -> renderWS "signup_error"
                  Just (token, uid) -> do
                    addFacilitatorPlace newPlace uid
                    redirectAsync $ BS.concat ["/activate/account?id=" 
                                              , uid
                                              , "&token="
                                              , token
                                              , "&pl="
                                              , pid
                                              ]
        
  
data SignupCreds = SignupCreds String String String

uniquePlace = checkM "Place with this name and organization already exists" $
                    \(SignupCreds o p _) -> fmap not $ placeExists (B8.pack o) (B8.pack p)

validOrg mu = case mu of
                Nothing -> checkM "Must be logged in as a facilitator to add to existing organizations" $ 
                                  \o -> fmap not $ organizationExists (B8.pack o)
                Just u -> checkM "Must be a facilitator for this organization to add a new place" $ 
                                  \o -> do exists <- organizationExists (B8.pack o)
                                           case exists of
                                             True -> return $ (B8.pack o) `elem` (map pOrg $ filter pFac (uPlaces u))
                                             False -> return True

signupForm :: Maybe User -> SnapForm Application Text HeistView SignupCreds
signupForm mu = (`validate` uniquePlace) $ (<++ errors) $ SignupCreds 
    <$> input "organization" Nothing `validate` nonEmpty `validate` (validOrg mu) <++ errors
    <*> input "place" Nothing `validate` nonEmpty <++ errors
    <*> input "name" Nothing `validate` (nonEmptyIfNothing mu) <++ errors
 
activateAccountH = do
  i <- getParam "id"
  tok <- getParam "token"
  pl <- getParam "pl"
  em <- getParam "em"
  muser <- maybe (return Nothing) (getUser) i 
  mplace <- maybe (return Nothing) getPlaceFromId pl
  wsPerformLogout -- make sure they aren't signed in as anyone else
  case (mkUser muser,tok,mplace) of
    (Just u, Just token, Just place) -> do 
      r <- eitherSnapForm newPasswordForm "change-password-form"
      case r of
          Left splices' -> do
            heistLocal (bindSplices splices') $ renderWS "activate/account"
          Right (p,_) -> do
            success <- activateAccount u p
            case success of
              True  -> do
                case em of
                  Nothing -> return False
                  Just email -> addUserEmail' u email
                redirect $ placeRoot place
              False -> -- don't know how this happened. Let's show the form again.
                      renderWS "activate/account"
    _ -> pass