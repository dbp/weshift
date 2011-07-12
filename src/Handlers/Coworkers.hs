{-# LANGUAGE OverloadedStrings #-}

module Handlers.Coworkers where

import Data.Maybe (fromJust, isNothing)  
import Control.Monad (when)

import Text.Templating.Heist
import Snap.Extension.Heist
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T  
import qualified Data.ByteString.Char8 as B8  
import Snap.Types

import State.Types
import State.Coworkers
import Application
import Common
import Auth

renderCoworker :: User -> Splice Application
renderCoworker (User uid uname uact usuper uplaces uview) = do
  runChildrenWithText [("name",       TE.decodeUtf8 uname)
                      ,("classes",    T.concat (["member"] ++ if pFac (head uplaces) then [" facilitator"] else []))
                      ]
                       
renderCoworkers :: [User] -> Splice Application
renderCoworkers coworkers = mapSplices renderCoworker coworkers


coworkersH :: User -> UserPlace -> Application ()
coworkersH user place = do 
  coworkers <- getCoworkers user place  -- Note: this gives back incomplete place lists, just the current place
  setView user "profile" "profile.coworkers"
  heistLocal (bindSplices (coworkersSplice coworkers)) $ renderWS "profile/coworkers/default"

coworkersSplice cs = [("coworkersCount", textSplice $ T.pack $ show $ length cs)
                     ,("coworkers", renderCoworkers cs)]