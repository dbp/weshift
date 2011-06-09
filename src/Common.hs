{-# LANGUAGE OverloadedStrings #-}

module Common where

import Text.Templating.Heist
import qualified Text.XmlHtml as X
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Snap.Extension.Heist
import Data.ByteString (ByteString)
import Snap.Auth.Handlers
import Snap.Auth
import Snap.Extension.Session.CookieSession
import Snap.Extension.Heist
import Control.Monad.Trans (lift, liftIO)
import Heist.Splices.Async (heistAsyncSplices)
import Data.Maybe (fromMaybe, maybeToList, listToMaybe)
import Control.Monad (liftM)
import Data.List (find)
import Snap.Types

import Data.Time.Format
import Data.Time.Clock
import Data.Time.Calendar
import System.Locale

import Application 
import Auth
import State.Types

placeName place = BS.intercalate ", " $ (map ($ place) [pName,pOrg])
placeRoot place = BS.intercalate "/"  $ (map (repUnders. ($ place)) [const "",pOrg,pName])

getCurrentPlace :: Application (Maybe UserPlace)
getCurrentPlace = do mplaceId <- getFromSession "place"
                     muser <- getCurrentUser
                     let hm = do placeId <- mplaceId
                                 user <- muser
                                 place <- find ((==) placeId . pId) $ uPlaces user
                                 return $ place
                     return hm

redirPlaceHome :: Application ()
redirPlaceHome = do hm <- liftM (fmap placeRoot) getCurrentPlace
                    redirect $ fromMaybe "/" hm

spliceMBS :: T.Text -> Maybe BS.ByteString -> [(T.Text, Splice Application)]
spliceMBS name val = maybeToList $ fmap (\p -> (name, return [X.TextNode (TE.decodeUtf8 p)])) val

facilitatorSplice = do node <- getParamNode
                       p <- lift getCurrentPlace
                       case p of
                         Just place -> if (pFac place) then return (X.elementChildren node) else return []
                         Nothing -> return []
                         
normalUserSplice = do node <- getParamNode
                      p <- lift getCurrentPlace
                      case p of
                        Just place -> if (not $ pFac place) then return (X.elementChildren node) else return []
                        Nothing -> return []

identitySplice :: TemplateMonad Application [X.Node]
identitySplice = do n <- getParamNode
                    return $ X.elementChildren n

parseWSDate s = parseTime defaultTimeLocale "%m.%d.%Y" $ B8.unpack s
                         
-- stolen from cgi:
maybeRead :: Read a => ByteString -> Maybe a
maybeRead = fmap fst . listToMaybe . reads . B8.unpack

renderWS :: ByteString -> Application ()
renderWS t = do mplace <- liftM (fmap placeRoot) getCurrentPlace
                mplaceName <- liftM (fmap placeName) getCurrentPlace
                let placeSplices = concat $ map (uncurry spliceMBS) [("placeRoot", mplace),("placeName", mplaceName)]
                muserName <- liftM (fmap uName) getCurrentUser
                let userSplices = concat $ map (uncurry spliceMBS) [("userName",muserName)]
                let permissionSplices = [("isFacilitator", facilitatorSplice),("isNormalUser", normalUserSplice)]
                (heistLocal $ (bindSplices (splices ++ placeSplices ++ userSplices))) $ render t
  where splices = [ ("ifLoggedIn", ifLoggedIn)
                  , ("ifGuest", ifGuest)
                  ] ++ heistAsyncSplices
                  
                  
redirTo :: Application ()
redirTo = do r <- getParam "redirectTo"
             redirect $ fromMaybe "/" r