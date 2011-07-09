{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Mail where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Attoparsec as A
import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as LBS 
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (fromJust)
import Network.HTTP hiding (getRequest)
import Network.HTTP.Base
import Network.HTTP.Headers
import Network.URI (parseURI)
import Secrets (postmarkToken)
import "mtl" Control.Monad.Trans

import Snap.Types hiding (POST)

import Network.Mail.Postmark

mailActivation account emails = do
  server  <- liftM rqServerName getRequest
  portNum <- liftM rqServerPort getRequest
  liftIO $ mapM (postmark (B8.unpack postmarkToken) "messages@weshift.com" "Please confirm your email with WeShift." "confirm" (BS.concat $ msg server portNum)) emails
    where msg s p = ["Please confirm your email account on WeShift. "
                  ,"To do that, visit "
                  ,"http://"
                  ,s
                  ,if p /= 80 then (B8.pack $ ':' : (show p)) else ""
                  ,"/activate?account="
                  ,account
                  ,"&token="
                  ,postmarkToken
                  ," .\n\nThanks! - The WeShift Team"]

resetPassword email token = undefined