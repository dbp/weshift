{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Handlers.Account where

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
import State.Types
import State.Place
import State.Account
import Handlers.Settings


loginGetH _ = do pl <- getParam "pl" -- this is the place
                 place <- maybe (return Nothing) getPlaceFromId pl
                 when (isNothing place) $ redirect "/"
                 rq <- getRequest
                 heistLocal (bindStrings [("placeName", TE.decodeUtf8 $ placeName (fromJust place))
                                         ,("placeRoot", TE.decodeUtf8 $ placeRoot (fromJust place))
                                         ,("placeId", TE.decodeUtf8 $ fromJust pl)
                                         ,("url", TE.decodeUtf8 $ rqURI rq)]) $ renderWS "login"
                                    

loginPostH loginFailure loginSuccess = do
    pl <- getParam "pl" -- this is the id of the place. If missing, this is a frontpage login,
                        -- which means we either guess the place (if name is unique and there is
                        -- only one place for them), or present a list to pick from.
    case pl of
      Nothing -> do
        pl' <- getParam "pl-ajax"
        case pl' of
          Nothing -> do
            name <- getParam "name"
            password <- getParam "password"
            places <- maybe (return []) getPlacesForName name
            case places of
              [] -> doLogin loginFailAjax (redirectAsync "/") -- nothing to do, this should error out on it's own
              (p:[]) -> do modifyRequest (rqSetParam "pl" [pId p]) -- only one place, so use it
                           doLogin loginFailAjax (redirectAsync (placeRoot p))
              _ -> do -- many places, which may be all of theirs, or may belong to multiple people with the same name; it doesnt matter.
                heistLocal (bindSplices [("name-value", textSplice $ TE.decodeUtf8 $ fromMaybe "" name)
                                        ,("password-value", textSplice $ TE.decodeUtf8 $ fromMaybe "" password)
                                        ,("places", renderPlaces Nothing places)
                                        ,("ifPlaces", identitySplice)
                                        ]) $ renderWS "login-form"
          Just pid -> do -- this is the case where they selected the place on the front page
            modifyRequest (rqSetParam "pl" [pid])
            mplace <- getPlaceFromId pid
            doLogin loginFailAjax (redirectAsync (maybe "/" placeRoot mplace))
      Just _ -> do
        doLogin loginFailure loginSuccess
  where doLogin failure success = do
          euid <- getParams
          password <- getParam "password"
          mMatch <- case password of
            Nothing -> return $ Left "Password Failure"
            Just p  -> performLogin euid
          either failure (const success) mMatch
        loginFailAjax = const $ heistLocal (bindSplices [("ifPlaces", blackHoleSplice), ("message", textSplice "Invalid username or password.")]) $ renderWS "login-form"

logoutH redirTo = do
  wsPerformLogout
  redirTo


signupH = do
  u <- getCurrentUser
  (view, result) <- runForm "signup-form" (signupForm u)
  case result of
      Nothing -> do
        heistLocal (bindDigestiveSplices view) $ renderWS "signup_form"
      Just (SignupCreds org place name) -> do
        createOrganizationIfNotExists (TE.encodeUtf8 org)
        mpid <- createPlace (TE.encodeUtf8 org) (TE.encodeUtf8 place)
        case mpid of
          Nothing -> renderWS "signup_error"
          Just pid -> do
            let newPlace = (emptyUserPlace {pId = pid, pName = (TE.encodeUtf8 place), pOrg = (TE.encodeUtf8 org), pFac = True})
            case u of
              Just user -> do -- an existing user
                addFacilitatorPlace newPlace (uId user)
                redirectAsync (placeRoot newPlace)
              Nothing -> do -- this is a new user
                mit <- newUser (TE.encodeUtf8 name) newPlace
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
        
  
data SignupCreds = SignupCreds Text Text Text

uniquePlace = checkM "Place with this name and organization already exists" $
                    \(SignupCreds o p _) -> fmap not $ placeExists (TE.encodeUtf8 o) (TE.encodeUtf8 p)

validOrg mu = case mu of
                Nothing -> checkM "Must be logged in as a facilitator to add to existing organizations" $ 
                                  \o -> fmap not $ organizationExists (TE.encodeUtf8 o)
                Just u -> checkM "Must be a facilitator for this organization to add a new place" $ 
                                  \o -> do exists <- organizationExists (TE.encodeUtf8 o)
                                           case exists of
                                             True -> return $ (TE.encodeUtf8 o) `elem` (map pOrg $ filter pFac (uPlaces u))
                                             False -> return True

--signupForm :: Maybe User -> SnapForm AppHandler Text HeistView SignupCreds
signupForm mu = uniquePlace $ SignupCreds 
    <$> "organization" .: (validOrg mu $ nonEmpty (text Nothing))
    <*> "place" .: nonEmpty (text Nothing)
    <*> "name" .: (nonEmptyIfNothing mu (text Nothing))
 
activateAccountH = do
  i <- getParam "id"
  tok <- getParam "token"
  pl <- getParam "pl"
  em <- getParam "em"
  muser <- maybe (return Nothing) (getUser) i 
  mplace <- maybe (return Nothing) getPlaceFromId pl
  wsPerformLogout -- make sure they aren't signed in as anyone else
  case (muser,tok,mplace) of
    (Just u, Just token, Just place) -> do 
      (view, result) <- runForm "change-password-form" newPasswordForm
      case result of
          Nothing -> do
            heistLocal (bindDigestiveSplices view) $ renderWS "activate/account"
          Just (p,_) -> do
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