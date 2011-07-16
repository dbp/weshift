{-# LANGUAGE OverloadedStrings #-}

module State.Types where


import            Control.Monad
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Maybe (catMaybes, listToMaybe)
import            Database.HDBC
import            Database.HDBC.SqlValue ()
import            Data.Time.LocalTime
import            Data.Time.Calendar
import            Data.Time.Clock

import Application


data User = User { uId :: BS.ByteString
                 , uName :: BS.ByteString
                 , uActive :: Bool
                 , uSuper :: Bool
                 , uPlaces :: [UserPlace]
                 , uView :: BS.ByteString
                 }
              deriving (Eq, Show)

emptyUser = User "" "" False False [] ""

             
data UserPlace = UserPlace { pId    :: BS.ByteString 
                           , pName  :: BS.ByteString 
                           , pOrg   :: BS.ByteString
                           , pFac   :: Bool 
                           , pToken :: BS.ByteString
                           }
      deriving (Eq, Show)

emptyUserPlace = UserPlace "" "" "" False "" 

data Email = Email { emId :: BS.ByteString
                   , emUser :: BS.ByteString
                   , emAddress :: BS.ByteString
                   , emConfirmed :: Bool
                   }
      deriving (Eq, Show)
      
data Attrs = Attrs Bool Bool [UserPlace] BS.ByteString
      deriving (Eq, Show)

emptyLocalTime = utcToLocalTime utc (UTCTime (fromGregorian 0 0 0) (fromInteger 0))      
      
data Shift = Shift { sId :: BS.ByteString
                   , sUser :: BS.ByteString
                   , sPlace :: BS.ByteString
                   , sStart :: LocalTime
                   , sStop :: LocalTime
                   , sRecorded :: LocalTime
                   , sRecorder :: BS.ByteString
                   }
                   deriving (Eq, Show, Read)
emptyShift = Shift "" "" "" emptyLocalTime emptyLocalTime emptyLocalTime ""


buildPlace (pi:pn:pt:po:[]) = Just $ UserPlace (fromSql pi) (fromSql pn) (fromSql po) False (fromSql pt) 
buildPlace _ = Nothing

buildShift (si:su:sp:ss:st:sr:sb:[]) = 
  Just $ Shift (fromSql si) (fromSql su) (fromSql sp) (fromSql ss) (fromSql st) (fromSql sr) (fromSql sb)
buildShift _ = Nothing

data Message = Message { mId :: BS.ByteString
                       , mContents :: BS.ByteString
                       , mUps :: Int
                       , mDowns :: Int
                       , mFlags :: Int
                       , mCreated :: LocalTime
                       }
                       
buildMessage (i:c:u:d:f:r:[]) = Just $ Message (fromSql i) (fromSql c) (fromSql u) (fromSql d) (fromSql f) (fromSql r)
buildMessage _ = Nothing