{-# LANGUAGE OverloadedStrings #-}

module Common where

import Text.Templating.Heist
import qualified Text.XmlHtml as X
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import Snap.Extension.Heist
import Data.ByteString (ByteString)
import Snap.Auth.Handlers
import Snap.Auth
import Snap.Extension.Session.CookieSession
import Snap.Extension.Heist
import Control.Monad.Trans (lift)
import Heist.Splices.Async (heistAsyncSplices)
import Data.Maybe (fromMaybe, maybeToList)
import Data.List (find)
import Snap.Types

import Application 
import Auth
import State

placeName place = TE.decodeUtf8 (BS.intercalate ", " $ (map ($ place) [pName,pOrg]))
placeRoot place = TE.decodeUtf8 (BS.intercalate "/"  $ (map (repUnders. ($ place)) [const "",pOrg,pName]))

renderWS :: ByteString -> Application ()
renderWS t = do mplaceId <- getFromSession "place"
                muser <- currentUser
                let placeSplice = maybeToList $ do placeId <- mplaceId
                                                   user <- muser
                                                   place <- find ((==) placeId . pId) $ uPlaces user
                                                   return $ ("placeRoot", return [X.TextNode (placeRoot place)])
                (heistLocal $ (bindSplices (splices ++ placeSplice))) $ render t
  where splices = [ ("ifLoggedIn", ifLoggedIn)
                  , ("ifGuest", ifGuest)]
                  ++ heistAsyncSplices
                  
                  
redirTo :: Application ()
redirTo = do r <- getParam "redirectTo"
             redirect $ fromMaybe "/" r