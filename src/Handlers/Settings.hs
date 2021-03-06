{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Handlers.Settings where

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
import           Forms.Settings
import           Mail
import           State.Account
import           State.Coworkers
import           State.Place

settingsH :: User -> UserPlace -> AppHandler ()
settingsH u p = route [ ("/",                 ifTop $ settingsHome u)
                      , ("/user",             userSettingsH u p)
                      , ("/name",             method POST $ changeNameH u p)
                      , ("/facilitation",     method POST $ stopFacilitatingH u p)
                      , ("/password",         changePasswordH u p)
                      , ("/remove",           method POST $ removeAccountPostH u p)
                      , ("/remove",           method GET $ removeAccountH u p)
                      , ("/email",            emailH u p)
                      , ("/email/add",        addEmailH u p)
                      , ("/email/delete/:id", deleteEmailH u p)
                      ]

settingsHome :: User -> AppHandler ()
settingsHome u = do setView u "profile" "profile.settings"
                    renderWS "profile/usersettings/blank"

userSettingsH u p = do setView u "profile" "profile.settings.name"
                       view <- getForm "ws" $ nameForm $ (Just . uName) u
                       heistLocal (bindDigestiveSplices view) $ renderWS "profile/usersettings/user"


changeNameH u p = do let name = uName u
                     (view, result) <- wsForm $ nameForm (Just name)
                     case result of
                         Nothing -> do
                            heistLocal (bindDigestiveSplices view) $ renderWS "profile/usersettings/name_form"
                         Just name' -> do
                            success <- fmap (not.null) $ withPGDB "UPDATE users SET name = ? WHERE id = ? RETURNING id;" [toSql name', toSql (uId u)]
                            case success of
                              True  -> renderWS "profile/usersettings/name_updated"
                              False -> renderWS "profile/usersettings/name_couldntupdate"

stopFacilitatingH u p = do
  numFacs <- getNumberFacilitators p
  case numFacs of
    Nothing -> userSettingsH u p
    -- can't be zero unless they are hacking, and so no need to give a user friendly message
    Just 1 -> renderWS "profile/usersettings/facilitation_only_one"
    Just _ -> do
      if pFac p then setFacilitator (uId u) p False else return False
      renderWS "profile/usersettings/facilitation_stopped"


changePasswordH u p = do (view, result) <- wsForm passwordForm
                         setView u "profile" "profile.settings.password"
                         case result of
                             Nothing -> do
                               heistLocal (bindDigestiveSplices view) $ renderWS "profile/usersettings/password"
                             Just (NewPassword _ new _) -> do
                               success <- fmap (not.null) $ withPGDB "UPDATE users SET password = crypt(?, gen_salt('bf')) WHERE id = ? RETURNING id;" [toSql new, toSql (uId u)]
                               case success of
                                 True  -> renderWS "profile/usersettings/password_updated"
                                 False -> renderWS "profile/usersettings/password_couldntupdate"

removeAccountH u p = do
  setView u "profile" "profile.settings.email"
  renderWS "profile/usersettings/remove"

removeAccountPostH :: User -> UserPlace -> AppHandler ()
removeAccountPostH u p = do
  mtok <- disableAccount u
  case mtok of
    Just (email, token) -> do
      mailDisabling (uId u) email (uName u) token (pId p)
      redirectAsync "/logout"
    Nothing -> heistLocal (bindSplices $ "msg" ## textSplice "Could not disable account.") $ renderWS "profile/usersettings/remove"


emailH u p = do
  setView u "profile" "profile.settings.email"
  emails <- getUserEmails u
  heistLocal (bindSplices ("emails" ## renderEmails emails)) $ renderWS "profile/usersettings/email"

renderEmails = mapSplices (\(Email i u a c) -> runChildrenWith $
                                               do "id" ## textSplice i
                                                  "user" ## textSplice u
                                                  "address" ## textSplice a
                                                  "confirmed" ## booleanSplice c)


addEmailH u p = do (view, result) <- wsForm emailForm
                   case result of
                       Nothing -> do
                         heistLocal (bindDigestiveSplices view) $ renderWS "profile/usersettings/email_add"
                       Just email' -> do
                         token <- addUserEmail u email'
                         case token of
                           Just t  -> do
                             mailEmailActivation (uId u) email' t (pId p)
                             emails <- getUserEmails u
                             heistLocal (bindSplices $ do "emails" ## renderEmails emails
                                                          "msg" ## textSplice "Confirmation email sent.") $ renderWS "profile/usersettings/email"
                           Nothing -> renderWS "profile/usersettings/email_error"

deleteEmailH u p = do
  mid <- fmap TE.decodeUtf8 <$> getParam "id"
  case mid of
    Just eid -> do deleteUserEmail u eid
                   emails <- getUserEmails u
                   heistLocal (bindSplices $ do "emails" ## renderEmails emails
                                                "msg" ## textSplice "Deleted address.") $ renderWS "profile/usersettings/email"
    Nothing -> do emails <- getUserEmails u
                  heistLocal (bindSplices ("emails" ## renderEmails emails)) $ renderWS "profile/usersettings/email"

activateEmail = do
  acc <- fmap TE.decodeUtf8 <$> getParam "account"
  emtok <- fmap TE.decodeUtf8 <$> getParam "token"
  pl <- fmap TE.decodeUtf8 <$> getParam "pl"
  case (acc,emtok,pl) of
    (Just uid, Just token, Just pid) -> do
      res <- confirmUserEmail uid token
      -- it actually doesn't matter for now, we'll redirect the same place,
      -- regardless of whether the email was succesfully activated
      mplace <- getPlaceFromId pid
      case mplace of
        Just place -> redirect $ TE.encodeUtf8 $ placeRoot place -- now this should end them up at the email panel,
                                                                 -- if they haven't gone anywhere since
        Nothing -> redirect "/" -- not sure how this could happen. I guess if they muck with the URL
    _ -> pass

activateDisabled = do
  acc <- fmap TE.decodeUtf8 <$> getParam "account"
  emtok <- fmap TE.decodeUtf8 <$> getParam "token"
  pl <- fmap TE.decodeUtf8 <$> getParam "pl"
  nam <- getParam "n"
  muser <- maybe (return Nothing) (getUser) acc
  mplace <- maybe (return Nothing) getPlaceFromId pl
  wsPerformLogout -- make sure they aren't signed in as anyone else
  case (muser,urlDecode =<< nam,emtok,mplace) of
    (Just u, Just name, Just token, Just place) -> do
      (view, result) <- wsForm newPasswordForm
      case result of
          Nothing -> do
            heistLocal (bindDigestiveSplices view) $ renderWS "activate/disabled"
          Just (p,_) -> do
            success <- enableAccount u (TE.decodeUtf8 name) p
            case success of
              True  -> redirect $ TE.encodeUtf8 $ placeRoot place
              False -> -- don't know how this happened. Let's show the form again.
                      renderWS "activate/disabled"
    _ -> pass
