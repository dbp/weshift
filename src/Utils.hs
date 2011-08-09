module Utils where

import Data.List (null, elemIndex)
import Test.QuickCheck
import Control.Monad
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.LocalTime
import System.Locale (defaultTimeLocale)
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)

eitherToMaybe = either (const Nothing) Just

findReplace fn val []     = val:[]
findReplace fn val (x:xs) = if fn x then val:xs else x : (findReplace fn val xs)

prop_findreplace_first :: [Int] -> Bool
prop_findreplace_first l = head (findReplace (const True) 1 l) == 1
prop_findreplace_last :: [Int] -> Bool
prop_findreplace_last l = 2 `elem` (findReplace (== 1) 2 newL)
  where newL = l ++ [1]
prop_findreplace_index :: [Int] -> Bool
prop_findreplace_index l = if null l then True else elemIndex fl newL >= elemIndex 2 (findReplace (== fl) 2 newL)
  where fl = head l
        newL = rotate l
        rotate l = (drop half l) ++ (take half l)
        half = (length l `div` 2)


wsFormatDay :: LocalTime -> ByteString
wsFormatDay = B8.pack . (formatTime defaultTimeLocale "%D")

wsFormatTime :: LocalTime -> ByteString
wsFormatTime  =  B8.pack . (formatTime defaultTimeLocale "%-I:%M%P")

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 = ((join .) .) . liftM2

bind3 :: Monad m => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
bind3 = (((join .) .) .) . liftM3

listToMaybeMany l@(_:_) = Just l
listToMaybeMany _       = Nothing

        
test = sequence (map quickCheck [prop_findreplace_first, prop_findreplace_last, prop_findreplace_index])