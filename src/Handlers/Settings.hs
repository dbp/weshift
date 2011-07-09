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
import Control.Applicative
import "mtl" Control.Monad.Trans (liftIO)

import Application
import Auth
import State.Types
import Common
import Control.Concurrent (threadDelay)

settingsH :: User -> UserPlace -> Application ()
settingsH u p = route [ ("/",         ifTop $ renderWS "profile/usersettings/blank")
                      , ("/name",     changeNameH u p)
                      , ("/password", changePasswordH u p)
                      , ("/remove",   removeAccountH u p)
                      , ("/email",    emailH u p)
                      ]

nameForm :: SnapForm Application Text HeistView String
nameForm = input "name" Nothing  `validate` nonEmpty <++ errors 
                  
changeNameH u p = do r <- eitherSnapForm nameForm "change-name-form"
                     let name = (TE.decodeUtf8 . uName) u
                     case r of
                         Left splices' -> do
                           heistLocal (bindString "name" name ) $ 
                            heistLocal (bindSplices splices') $ renderWS "profile/usersettings/name"
                         Right name' -> do
                           case u of
                             (User id' _ _ _ _) -> do
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
                    Just (User id' _ _ _ _) ->
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
                         case r of
                             Left splices' -> do
                               heistLocal (bindSplices splices') $ renderWS "profile/usersettings/password"
                             Right (NewPassword _ new _) -> do
                               case u of
                                 (User id' _ _ _ _) -> do
                                   success <- fmap (not.null) $ withPGDB "UPDATE users SET password = crypt(?, gen_salt('bf')) WHERE id = ? RETURNING id;" [toSql new, toSql id']
                                   case success of
                                     True  -> renderWS "profile/usersettings/password_updated"
                                     False -> renderWS "profile/usersettings/password_couldntupdate"

removeAccountH u p = undefined
emailH u p = undefined