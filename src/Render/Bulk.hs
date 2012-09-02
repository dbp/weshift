{-# LANGUAGE OverloadedStrings #-}
         
module Render.Bulk where

-- | Boilerplate imports
import            Imports
import qualified  Data.Text as T
import qualified  Data.Text.Encoding as TE
import qualified  Data.Bson as B
import qualified  Data.Map as M
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import qualified  Text.XmlHtml as X
import qualified  Utils as U

-- | Module Specific imports
import Text.SSV

renderNotUnderstood :: Monad m => [(String,Day,String)] -> Splice m
renderNotUnderstood = mapSplices (\(name, date, shift) -> 
                           runChildrenWithText [("name",        T.pack name)
                                               ,("date",        T.pack $ show date)
                                               ,("shift",       T.pack shift)
                                               ])

renderNotUnderStoodCSV :: [(String,Day,String)] -> String
renderNotUnderStoodCSV nus = showCSV $ rows
  where days = nub $ map (\(_,d,_) -> d) nus
        names = nub $ map (\(n,_,_) -> n) nus
        blanks = map (\n -> (n,days)) names
        rows = ("Name":(map (formatTime defaultTimeLocale "%-m/%-d/%Y") days)) : (map (\(n, days) -> n : (map (gets n) days)) blanks)
        gets n d = maybe "" (\(_,_,s) -> s) $ find (\(n',d',s) -> n==n' && d==d') nus
