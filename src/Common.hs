{-# LANGUAGE OverloadedStrings #-}

module Common where

import Text.Templating.Heist
import qualified Text.XmlHtml as X
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Map as M
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
import State.Coworkers

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

renderTime t = T.pack $ formatTime defaultTimeLocale "%-l:%M%P" t
renderDate t = T.pack $ formatTime defaultTimeLocale "%m.%d.%Y" t
renderDateLong t = T.pack $ formatTime defaultTimeLocale "%B %e, %Y" t

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


identitySplice :: Monad m => Splice m
identitySplice = do node <- getParamNode
                    return (X.elementChildren node)
                    
blackHoleSplice :: Monad m => Splice m
blackHoleSplice = return []

parseWSDate s = parseTime defaultTimeLocale "%m.%d.%Y" $ B8.unpack s
                         
-- stolen from cgi:
maybeRead :: Read a => ByteString -> Maybe a
maybeRead = fmap fst . listToMaybe . reads . B8.unpack


nameLookup :: M.Map BS.ByteString User -> Splice Application
nameLookup names = do node <- getParamNode
                      case X.getAttribute "lookup" node >>= (flip M.lookup names . TE.encodeUtf8) of
                        Nothing -> return []
                        Just user -> runChildrenWith [("id", textSplice $ TE.decodeUtf8 $ uId user)
                                                     ,("name", textSplice $ TE.decodeUtf8 $ uName user)
                                                     ,("active", if uActive user then identitySplice else blackHoleSplice)
                                                     ,("super",  if uSuper user then identitySplice else blackHoleSplice)
                                                     ]  

renderWS :: ByteString -> Application ()
renderWS t = do mplace <- getCurrentPlace
                let mplaceRoot = fmap placeRoot mplace
                let mplaceName = fmap placeName mplace
                let placeSplices = concat $ map (uncurry spliceMBS) [("placeRoot", mplaceRoot),("placeName", mplaceName)]
                muserName <- liftM (fmap uName) getCurrentUser
                let userSplices = concat $ map (uncurry spliceMBS) [("userName",muserName)]
                let permissionSplices = [("isFacilitator", facilitatorSplice),("isNormalUser", normalUserSplice)]
                workersSplice <- case mplace of
                                    Nothing -> return []
                                    Just place -> do us <- getWorkers place
                                                     return [("user", nameLookup $ M.fromList $ map (\u -> (uId u, u)) us)]
                (heistLocal $ (bindSplices (splices ++ placeSplices ++ userSplices ++ workersSplice))) $ render t
  where splices = [ ("ifLoggedIn", ifLoggedIn)
                  , ("ifGuest", ifGuest)
                  ] ++ heistAsyncSplices
                  
                  
redirTo :: Application ()
redirTo = do r <- getParam "redirectTo"
             redirect $ fromMaybe "/" r