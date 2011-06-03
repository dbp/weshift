{-# LANGUAGE OverloadedStrings #-}

module Common where

import Text.Templating.Heist
import Snap.Extension.Heist
import Data.ByteString (ByteString)
import Snap.Auth.Handlers
import Snap.Auth
import Snap.Extension.Heist
import Control.Monad.Trans (lift)
import Heist.Splices.Async (heistAsyncSplices)
import Data.Maybe (fromMaybe)
import Snap.Types

import Application 
import Auth


renderWS :: ByteString -> Application ()
renderWS = (heistLocal $ (bindSplices splices)) . render
  where splices = [ ("ifLoggedIn", ifLoggedIn)
                  , ("ifGuest", ifGuest)]
                  ++ heistAsyncSplices
                  
                  
redirTo :: Application ()
redirTo = do r <- getParam "redirectTo"
             redirect $ fromMaybe "/" r