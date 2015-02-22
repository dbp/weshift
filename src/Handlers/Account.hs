{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Handlers.Account where

-- | Boilerplate imports
import qualified Data.Bson             as B
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map              as M
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Imports
import qualified Text.XmlHtml          as X
import qualified Utils                 as U

-- | Module specific imports
import           Forms.Account
import           Handlers.Settings
import           State.Account
import           State.Place

loginGetH _ = do pl <- fmap TE.decodeUtf8 <$> getParam "pl" -- this is the place
                 place <- maybe (return Nothing) getPlaceFromId pl
                 when (isNothing place) $ redirect "/"
                 rq <- getRequest
                 heistLocal (bindStrings $
                             do "placeName" ## placeName (fromJust place)
                                "placeRoot" ## placeRoot (fromJust place)
                                "placeId" ## fromJust pl
                                "url" ## TE.decodeUtf8 (rqURI rq)) $ renderWS "login"


loginPostH loginFailure loginSuccess = do
    pl <- fmap TE.decodeUtf8 <$> getParam "pl" -- this is the id of the place. If missing, this is a frontpage login,
                        -- which means we either guess the place (if name is unique and there is
                        -- only one place for them), or present a list to pick from.
    case pl of
      Nothing -> do
        pl' <- fmap TE.decodeUtf8 <$> getParam "pl-ajax"
        case pl' of
          Nothing -> do
            name <- fmap TE.decodeUtf8 <$> getParam "name"
            password <- fmap TE.decodeUtf8 <$> getParam "password"
            places <- maybe (return []) getPlacesForName name
            case places of
              [] -> doLogin loginFailAjax (redirectAsync "/") -- nothing to do, this should error out on it's own
              (p:[]) -> do modifyRequest (rqSetParam "pl" [TE.encodeUtf8 $ pId p]) -- only one place, so use it
                           doLogin loginFailAjax (redirectAsync (placeRoot p))
              _ -> do -- many places, which may be all of theirs, or may belong to multiple people with the same name; it doesnt matter.
                heistLocal (bindSplices $ do "name-value" ## textSplice $ fromMaybe "" name
                                             "password-value" ## textSplice $ fromMaybe "" password
                                             "places" ## renderPlaces Nothing places
                                             "ifPlaces" ## identitySplice
                                        ) $ renderWS "login-form"
          Just pid -> do -- this is the case where they selected the place on the front page
            modifyRequest (rqSetParam "pl" [TE.encodeUtf8 pid])
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
        loginFailAjax = const $ heistLocal (bindSplices $ do "ifPlaces" ## blackHoleSplice
                                                             "message" ## textSplice "Invalid username or password.") $ renderWS "login-form"

logoutH redirTo = do
  wsPerformLogout
  redirTo


signupH = do
  u <- getCurrentUser
  (view, result) <- wsForm (signupForm u)
  case result of
      Nothing -> do
        heistLocal (bindDigestiveSplices view) $ renderWS "signup_form"
      Just (SignupCreds org place name) -> do
        createOrganizationIfNotExists org
        mpid <- createPlace org place
        case mpid of
          Nothing -> renderWS "signup_error"
          Just pid -> do
            let newPlace = (emptyUserPlace {pId = pid, pName = place, pOrg = org, pFac = True})
            case u of
              Just user -> do -- an existing user
                addFacilitatorPlace newPlace (uId user)
                redirectAsync (placeRoot newPlace)
              Nothing -> do -- this is a new user
                mit <- newUser name newPlace
                case mit of
                  Nothing -> renderWS "signup_error"
                  Just (token, uid) -> do
                    addFacilitatorPlace newPlace uid
                    redirectAsync $ T.concat ["/activate/account?id="
                                             , uid
                                             , "&token="
                                             , token
                                             , "&pl="
                                             , pid
                                             ]

activateAccountH = do
  i <- fmap TE.decodeUtf8 <$> getParam "id"
  tok <- fmap TE.decodeUtf8 <$> getParam "token"
  pl <- fmap TE.decodeUtf8 <$> getParam "pl"
  em <- fmap TE.decodeUtf8 <$> getParam "em"
  muser <- maybe (return Nothing) (getUser) i
  mplace <- maybe (return Nothing) getPlaceFromId pl
  wsPerformLogout -- make sure they aren't signed in as anyone else
  case (muser,tok,mplace) of
    (Just u, Just token, Just place) -> do
      (view, result) <- wsForm newPasswordForm
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
                redirect $ TE.encodeUtf8 $ placeRoot place
              False -> -- don't know how this happened. Let's show the form again.
                      renderWS "activate/account"
    _ -> pass
