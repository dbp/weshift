{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Handlers.Day where
  
import Snap.Types

import Text.Templating.Heist
import Snap.Extension.Heist
import qualified Text.XmlHtml as X

import Data.Maybe (fromJust, fromMaybe)

import Text.Digestive.Types
import Text.Digestive.Snap.Heist
import Text.Digestive.Validate
import Database.HDBC

import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Control.Applicative
import "mtl" Control.Monad.Trans (liftIO, lift)

import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Calendar.OrdinalDate
import Data.List.Split

import Application
import Auth
import State.Types
import State.Shifts
import State.Coworkers
import Handlers.Shifts
import Common

dayView :: User -> UserPlace -> Integer -> Int -> Int -> Splice Application
dayView u p year month day = do
  let start = fromGregorian year month day 
  let end = addDays 1 start
  shifts <- lift $ getShifts start end p
  uncoveredShifts <- lift $ getUncoveredShifts start end p
  workers <- lift $ getWorkers p
  let shiftUsers = map sUser shifts
  let dayLaborers = filter (\w -> (uId w) `elem` shiftUsers) workers
  renderDayVD shifts uncoveredShifts dayLaborers
  
renderDayVD s us ds = mapSplices (workerDay s us) ds

workerDay ss us dl = runChildrenWith [("name", textSplice $ TE.decodeUtf8 $ uName dl)
                                     ,("shifts", mapSplices renderShift (filter (\s -> sUser s == uId dl) ss))
                                     {-,()-}
                                     ]  