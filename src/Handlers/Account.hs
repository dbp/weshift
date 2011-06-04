{-# LANGUAGE OverloadedStrings #-}

module Handlers.Account where

import Data.Maybe (isNothing, fromJust, listToMaybe)  
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE

import Snap.Types
import Common
import Application
import Auth
import State
import Text.Templating.Heist
import Snap.Extension.Heist
import Control.Monad

import qualified Snap.Auth as A


loginGetH _ = do pl <- getParam "pl" -- this is the place
                 place <- maybe (return Nothing) getPlaceFromId pl
                 when (isNothing place) $ redirect "/"
                 let placeName = TE.decodeUtf8 (BS.intercalate ", " $ (map ($ (fromJust place)) [pName,pOrg]))
                 heistLocal (bindString "placeName" placeName) $ render "login"
                                    

loginPostH loginFailure loginSuccess = do
    euid <- getParams
    password <- getParam "password"
    mMatch <- case password of
      Nothing -> return $ Left A.PasswordFailure
      Just p  -> performLogin euid
    either loginFailure (const loginSuccess) mMatch

signupH = undefined