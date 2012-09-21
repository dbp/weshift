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

import            Application


data User = User { uId :: BS.ByteString
                 , uName :: BS.ByteString
                 , uActive :: Bool
                 , uSuper :: Bool
                 , uPlaces :: [UserPlace]
                 , uView :: BS.ByteString
                 , uToken :: BS.ByteString
                 }
              deriving (Eq, Show)

emptyUser = User "" "" False False [] "" ""

             
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
      
data Attrs = Attrs Bool Bool [UserPlace] BS.ByteString BS.ByteString
      deriving (Eq, Show)

emptyLocalTime = utcToLocalTime utc (UTCTime (fromGregorian 0 0 0) (fromInteger 0))      

data Color = Blue | Red | Green | Transparent deriving (Eq, Show, Read)

colorToInt :: Color -> Int
colorToInt Transparent = 0
colorToInt Blue = 1
colorToInt Red = 2
colorToInt Green = 3
colorFromInt :: Int -> Color
colorFromInt 1 = Blue
colorFromInt 2 = Red
colorFromInt 3 = Green
colorFromInt _ = Transparent

data Shift = Shift { sId :: BS.ByteString
                   , sUser :: BS.ByteString
                   , sPlace :: BS.ByteString
                   , sStart :: LocalTime
                   , sStop :: LocalTime
                   , sColor :: Color
                   , sUnits :: Double
                   , sRecorded :: LocalTime
                   , sRecorder :: BS.ByteString
                   }
                   deriving (Eq, Show, Read)
emptyShift = Shift "" "" "" emptyLocalTime emptyLocalTime Transparent 0 emptyLocalTime ""

data Modification = Delete User LocalTime -- who deleted it, and when
                  | Change LocalTime LocalTime Color Double User LocalTime
                  -- new start, new end, new color, new units who did it, when it was changed
                  | Cover User LocalTime -- who covered it, and when
                  deriving (Eq, Show)
mTime (Delete _ t) = t
mTime (Change _ _ _ _ _ t) = t
mTime (Cover _ t) = t

buildPlace (pi:pn:pt:po:[]) = Just $ UserPlace (fromSql pi) (fromSql pn) (fromSql po) False (fromSql pt) 
buildPlace _ = Nothing

buildShift (si:su:sp:ss:st:sr:sb:sc:sun:[]) = 
  Just $ Shift (fromSql si) (fromSql su) (fromSql sp) (fromSql ss) (fromSql st) 
               (colorFromInt (fromSql sc)) (fromSql sun)
               (fromSql sr) (fromSql sb)

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