{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module State where


import            Control.Monad
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Maybe (catMaybes, listToMaybe)
import            Database.HDBC

import Application


data User = User { uId :: BS.ByteString
                 , uName :: BS.ByteString
                 , uActive :: Bool
                 , uSuper :: Bool
                 , uPlaces :: [UserPlace]
                 }
              deriving (Eq, Show)
              
data UserPlace = UserPlace { pId    :: BS.ByteString 
                           , pName  :: BS.ByteString 
                           , pOrg   :: BS.ByteString
                           , pFac   :: Bool 
                           , pToken :: BS.ByteString
                           }
      deriving (Eq, Show)

data Attrs = Attrs Bool Bool [UserPlace]
      deriving (Eq, Show)

buildPlace (pi:pn:pt:po:[]) = Just $ UserPlace (fromSql pi) (fromSql pn) (fromSql po) False (fromSql pt) 
buildPlace _ = Nothing


getPlaceFromId :: BS.ByteString -> Application (Maybe UserPlace)
getPlaceFromId pid = fmap ((>>= buildPlace).listToMaybe) $ withPGDB "SELECT P.id, P.name, P.token, P.organization FROM places as P WHERE P.id = ?;" [toSql pid]
  
getPlace :: BS.ByteString -> BS.ByteString -> Application (Maybe UserPlace)
getPlace org place = do resp <- fmap listToMaybe $ withPGDB "SELECT P.id, P.name, P.token, P.organization FROM places as P WHERE P.name = ? AND P.organization = ?;" [toSql place, toSql org]
                        return $ buildPlace =<< resp

getUserPlaces :: BS.ByteString -> Application [[SqlValue]]
getUserPlaces uid = withPGDB "SELECT P.id, P.name, P.token, P.organization, PU.facilitator FROM places as P JOIN placeusers AS PU ON PU.place = P.id WHERE PU.user_id = ?;" [toSql uid]