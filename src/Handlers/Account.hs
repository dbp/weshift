{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Handlers.Account where

import Data.Maybe (isNothing, fromJust, listToMaybe)  
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE

import Snap.Types
import Common
import Application
import Auth
import State.Types
import State.Place
import Text.Templating.Heist
import Snap.Extension.Heist
import Control.Monad
import "mtl" Control.Monad.Trans

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

signupH = undefined