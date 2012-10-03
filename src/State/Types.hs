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

data Color = Red | Green | Orange | Purple | Tan | Lime | Blue | Transparent deriving (Eq, Show, Read)

colorToInt :: Color -> Int
colorToInt Transparent = 0
colorToInt Red = 1
colorToInt Green = 2
colorToInt Orange = 3
colorToInt Purple = 4
colorToInt Tan = 5
colorToInt Lime = 6
colorToInt Blue = 7

colorFromInt :: Int -> Color
colorFromInt 1 = Red
colorFromInt 2 = Green
colorFromInt 3 = Orange
colorFromInt 4 = Purple
colorFromInt 5 = Tan
colorFromInt 6 = Lime
colorFromInt 7 = Blue
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
                   , sDeadline :: Bool
                   , sDeadlineDone :: Bool
                   , sDescription :: BS.ByteString
                   , sClaims :: Bool
                   }
                   deriving (Eq, Show, Read)
emptyShift = Shift "" "" "" emptyLocalTime emptyLocalTime Transparent 0 emptyLocalTime "" False False "" False

data Claim = Claim { cId :: BS.ByteString
                   , cShift :: BS.ByteString
                   , cUser :: BS.ByteString
                   , cUnits :: Double
                   , cReason :: BS.ByteString
                   , cResolved :: Bool
                   , cAccepted :: Bool
                   }
                   deriving (Eq, Show)
emptyClaim = Claim "" "" "" 0 "" False False

data Modification = Delete User LocalTime -- who deleted it, and when
                  | Change LocalTime LocalTime Color Double User LocalTime BS.ByteString
                  -- new start, new end, new color, new units who did it, when it was changed, new desc
                  | Cover User LocalTime -- who covered it, and when
                  deriving (Eq, Show)
mTime (Delete _ t) = t
mTime (Change _ _ _ _ _ t _) = t
mTime (Cover _ t) = t

buildPlace (pi:pn:pt:po:[]) = Just $ UserPlace (fromSql pi) (fromSql pn) (fromSql po) False (fromSql pt) 
buildPlace _ = Nothing

buildShift (si:su:sp:ss:st:sr:sb:sc:sun:sd:sdd:sdesc:scls:[]) = 
  Just $ Shift (fromSql si) (fromSql su) (fromSql sp) (fromSql ss) (fromSql st) 
               (colorFromInt (fromSql sc)) (fromSql sun)
               (fromSql sr) (fromSql sb) (fromSql sd) (fromSql sdd) (fromSql sdesc)
               (fromSql scls)

buildShift _ = Nothing

buildClaim (ci:cs:cus:cun:cr1:cr2:ac:[]) = Just $ Claim (fromSql ci) (fromSql cs) (fromSql cus) (fromSql cun) (fromSql cr1) (fromSql cr2) (fromSql ac)
buildClaim _ = Nothing

data Message = Message { mId :: BS.ByteString
                       , mContents :: BS.ByteString
                       , mUps :: Int
                       , mDowns :: Int
                       , mFlags :: Int
                       , mCreated :: LocalTime
                       }
                       
buildMessage (i:c:u:d:f:r:[]) = Just $ Message (fromSql i) (fromSql c) (fromSql u) (fromSql d) (fromSql f) (fromSql r)
buildMessage _ = Nothing