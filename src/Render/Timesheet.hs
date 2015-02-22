{-# LANGUAGE OverloadedStrings #-}

module Render.Timesheet where

-- | Boilerplate imports
import qualified Data.Bson             as B
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map              as M
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Imports
import qualified Text.XmlHtml          as X
import qualified Utils                 as U

-- | Module Specific imports

renderTSCoworkers self coworkers = mapSplices (renderTSCoworker self) (self : coworkers)
renderTSCoworker self u = runChildrenWithText $ do "userId" ##  uId u
                                                   "userName" ##  uName u
                                                   "selected" ## if u == self then "selected='selected'" else ""


data Entry = Entry Double Double LocalTime LocalTime [Modification] Bool Bool T.Text deriving Show -- hours worked, units, orig. start, orig. end, list of modifications, is deadline, is deadline done, description

entryHours (Entry h _ _ _ _ _ _ _) = h
entryUnits (Entry _ u _ _ _ _ _ _) = u
entryDeadline (Entry _ _ _ _ _ d _ _) = d
entryDone (Entry _ _ _ _ _ _ d _) = d
entryDescription (Entry _ _ _ _ _ _ _ d) = d

renderChange d (Delete u t) = runChildrenWithText $ do "changeClasses" ## "delete"
                                                       "changeDescription" ## "Deleted"
                                                       "changePerson" ##  uName u
                                                       "changeTime" ## renderTime t
                                                       "changeDate" ## renderDate t


renderChange d (Change s e c un u t desc) =
  runChildrenWithText $ do "changeClasses" ## "change"
                           "changeDescription" ## T.concat ((if T.length desc /= 0 then ["'",  desc,"', "] else []) ++ [renderTime s] ++ (if d then [] else ["-",(renderTime e)]) ++ [" (", T.pack $ show un, ")"])
                           "changePerson" ##  uName u
                           "changeTime" ## renderTime t
                           "changeDate" ## renderDate t


renderChange d (Cover u t) = runChildrenWithText $ do "changeClasses" ## "cover"
                                                      "changeDescription" ## "Covered"
                                                      "changePerson" ## uName u
                                                      "changeTime" ## renderTime t
                                                      "changeDate" ## renderDate t

renderEntry (Entry hours units start end changes deadline deadlineDone description) =
  runChildrenWith $ do "hoursWorked" ## textSplice $ T.pack $ show hours
                       "units" ## textSplice $ T.pack $ show units
                       "startTime" ## textSplice $ renderTime start
                       "endTime" ## textSplice $ renderTime end
                       "shiftDate" ## textSplice $ renderDateLong start
                       "changes" ## mapSplices (renderChange deadline) changes
                       "isDeadline" ## booleanSplice deadline
                       "notDeadline" ## booleanSplice (not deadline)
                       "isDeadlineDone" ## booleanSplice (deadlineDone || (not deadline))
                       "notDeadlineDone" ## booleanSplice ((not deadlineDone) || (not deadline))
                       "description" ## textSplice description
