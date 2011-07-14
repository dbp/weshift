{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Handlers.Settings where
  
import Snap.Types

import Text.Templating.Heist
import Snap.Extension.Heist

import Text.Digestive.Types
import Text.Digestive.Snap.Heist
import Text.Digestive.Validate
import Database.HDBC

import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as B8
import Control.Applicative
import "mtl" Control.Monad.Trans (liftIO)

import Application
import Auth
import State.Types
import State.Account
import State.Place
import Common
import Mail
import Control.Concurrent (threadDelay)

settingsH :: User -> UserPlace -> Application ()
settingsH u p = route [ ("/",                 ifTop $ settingsHome u)
                      , ("/name",             changeNameH u p)
                      , ("/password",         changePasswordH u p)
                      , ("/remove",           removeAccountH u p)
                      , ("/email",            emailH u p)
                      , ("/email/add",        addEmailH u p)
                      , ("/email/delete/:id", deleteEmailH u p)
                      ]

settingsHome :: User -> Application ()
settingsHome u = do setView u "profile" "profile.settings"
                    renderWS "profile/usersettings/blank"


nameForm :: SnapForm Application Text HeistView String
nameForm = input "name" Nothing  `validate` nonEmpty <++ errors 
                  
changeNameH u p = do r <- eitherSnapForm nameForm "change-name-form"
                     let name = (TE.decodeUtf8 . uName) u
                     setView u "profile" "profile.settings.name"
                     case r of
                         Left splices' -> do
                           heistLocal (bindString "name" name ) $ 
                            heistLocal (bindSplices splices') $ renderWS "profile/usersettings/name"
                         Right name' -> do
                           case u of
                             (User id' _ _ _ _ _) -> do
                               success <- fmap (not.null) $ withPGDB "UPDATE users SET name = ? WHERE id = ? RETURNING id;" [toSql name', toSql id']
                               {-liftIO $ threadDelay 200000-}
                               case success of
                                 True  -> renderWS "profile/usersettings/name_updated"
                                 False -> renderWS "profile/usersettings/name_couldntupdate"


nonEmpty :: Validator Application Text String
nonEmpty = check "Field must not be empty:" $ \s -> not $ null s

checkPassword :: Validator Application Text String
checkPassword = checkM "Current password not correct:" fn
  where fn p = do mUs <- getCurrentUser
                  case mUs of
                    Nothing -> return False
                    Just (User id' _ _ _ _ _) ->
                      fmap (not.null) $ withPGDB 
                        "SELECT id FROM users WHERE id = ? AND password = crypt(?, password) LIMIT 1;" 
                        [toSql id', toSql p]                  


data NewPassword = NewPassword String String String deriving (Eq,Show)

passwordForm :: SnapForm Application Text HeistView NewPassword
passwordForm = (`validate` matchingPasswords) $ (<++ errors) $ NewPassword
    <$> input "current" Nothing  `validate` checkPassword <++ errors
    <*> input "new"     Nothing  `validate` nonEmpty      <++ errors 
    <*> input "confirm" Nothing  `validate` nonEmpty      <++ errors 
  where matchingPasswords = check "New passwords do not match:" $ \(NewPassword _ p1 p2) -> p1 == p2


changePasswordH u p = do r <- eitherSnapForm passwordForm "change-password-form"
                         setView u "profile" "profile.settings.password"
                         case r of
                             Left splices' -> do
                               heistLocal (bindSplices splices') $ renderWS "profile/usersettings/password"
                             Right (NewPassword _ new _) -> do
                               case u of
                                 (User id' _ _ _ _ _) -> do
                                   success <- fmap (not.null) $ withPGDB "UPDATE users SET password = crypt(?, gen_salt('bf')) WHERE id = ? RETURNING id;" [toSql new, toSql id']
                                   case success of
                                     True  -> renderWS "profile/usersettings/password_updated"
                                     False -> renderWS "profile/usersettings/password_couldntupdate"

removeAccountH u p = undefined

emailH u p = do
  setView u "profile" "profile.settings.email"
  emails <- getUserEmails u
  heistLocal (bindSplices [("emails", renderEmails emails)]) $ renderWS "profile/usersettings/email"

renderEmails = mapSplices (\(Email i u a c) -> runChildrenWith [("id", textSplice $ TE.decodeUtf8 i)
                                                               ,("user", textSplice $ TE.decodeUtf8 u)
                                                               ,("address", textSplice $ TE.decodeUtf8 a)
                                                               ,("confirmed", booleanSplice c)
                                                               ])

addEmailH u p = do r <- eitherSnapForm emailForm "add-email-form"
                   case r of
                       Left splices' -> do
                         heistLocal (bindSplices splices') $ renderWS "profile/usersettings/email_add"
                       Right email' -> do
                         token <- addUserEmail u email'
                         case token of
                           Just t  -> do
                             mailActivation (uId u) (B8.pack email') t (pId p)
                             emails <- getUserEmails u
                             heistLocal (bindSplices [("emails", renderEmails emails),("msg",textSplice "Confirmation email sent.")]) $ renderWS "profile/usersettings/email" 
                           Nothing -> renderWS "profile/usersettings/email_error"

deleteEmailH u p = do 
  mid <- getParam "id"
  case mid of
    Just eid -> do deleteUserEmail u eid
                   emails <- getUserEmails u
                   heistLocal (bindSplices [("emails", renderEmails emails),("msg",textSplice "Deleted address.")]) $ renderWS "profile/usersettings/email"
    Nothing -> do emails <- getUserEmails u
                  heistLocal (bindSplices [("emails", renderEmails emails)]) $ renderWS "profile/usersettings/email"

activateEmail = do
  acc <- getParam "account"
  emtok <- getParam "token"
  pl <- getParam "pl"
  case (acc,emtok,pl) of
    (Just uid, Just token, Just pid) -> do 
      res <- confirmUserEmail uid token
      -- it actually doesn't matter for now, we'll redirect the same place,
      -- regardless of whether the email was succesfully activated
      mplace <- getPlaceFromId pid
      case mplace of
        Just place -> redirect $ placeRoot place -- now this should end them up at the email panel, 
                                                 -- if they haven't gone anywhere since
        Nothing -> redirect "/" -- not sure how this could happen. I guess if they muck with the URL
    _ -> pass

                 
                         
-- | regex validation sucks, so don't even try.
validEmail :: Validator Application Text String
validEmail = check "Must be a valid email, like help@weshift.org" $ \e -> '@' `elem` e && '.' `elem` e

emailForm :: SnapForm Application Text HeistView String
emailForm = input "address" Nothing  `validate` validEmail <++ errors 
                  