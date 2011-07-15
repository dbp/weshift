{-# LANGUAGE OverloadedStrings #-}

module State.Place where


import            Control.Monad
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import            Database.HDBC
import            Data.Time.LocalTime

import Application
import State.Types

getPlaceFromId :: BS.ByteString -> Application (Maybe UserPlace)
getPlaceFromId pid = fmap ((>>= buildPlace).listToMaybe) $ withPGDB "SELECT P.id, P.name, P.token, P.organization FROM places as P WHERE P.id = ?;" [toSql pid]
  
getPlace :: BS.ByteString -> BS.ByteString -> Application (Maybe UserPlace)
getPlace org place = do resp <- fmap listToMaybe $ withPGDB "SELECT P.id, P.name, P.token, P.organization FROM places as P WHERE P.name = ? AND P.organization = ?;" [toSql place, toSql org]
                        return $ buildPlace =<< resp

getUserPlaces :: BS.ByteString -> Application [[SqlValue]]
getUserPlaces uid = withPGDB "SELECT P.id, P.name, P.token, P.organization, PU.facilitator FROM places as P JOIN placeusers AS PU ON PU.place = P.id WHERE PU.user_id = ?;" [toSql uid]

getAllPlaces :: Application [UserPlace]
getAllPlaces = fmap (mapMaybe buildPlace) $ withPGDB "SELECT id, name, token, organization FROM places;" []
