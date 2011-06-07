{-# LANGUAGE OverloadedStrings #-}

module State.Types where


import            Control.Monad
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Maybe (catMaybes, listToMaybe)
import            Database.HDBC
import            Data.Time.LocalTime

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
      
data Shift = Shift { sId :: BS.ByteString
                   , sUser :: BS.ByteString
                   , sPlace :: BS.ByteString
                   , sStart :: LocalTime
                   , sStop :: LocalTime
                   , sRecorded :: LocalTime
                   , sRecorder :: BS.ByteString
                   }

buildPlace (pi:pn:pt:po:[]) = Just $ UserPlace (fromSql pi) (fromSql pn) (fromSql po) False (fromSql pt) 
buildPlace _ = Nothing

buildShift (si:su:sp:ss:st:sr:sb:[]) = 
  Just $ Shift (fromSql si) (fromSql su) (fromSql sp) (fromSql ss) (fromSql st) (fromSql sr) (fromSql sb)
buildShift _ = Nothing