{-# LANGUAGE OverloadedStrings #-}

module Render.Timesheet where

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

renderTSCoworkers self coworkers = mapSplices (renderTSCoworker self) (self : coworkers)
renderTSCoworker self u = runChildrenWithText [ ("userId",   TE.decodeUtf8 $ uId u)
                                              , ("userName", TE.decodeUtf8 $ uName u)
                                              , ("selected", if u == self then "selected='selected'" else "")
                                              ]


data Entry = Entry Double Double LocalTime LocalTime [Modification] Bool Bool BS.ByteString deriving Show -- hours worked, units, orig. start, orig. end, list of modifications, is deadline, is deadline done, description

entryHours (Entry h _ _ _ _ _ _ _) = h
entryUnits (Entry _ u _ _ _ _ _ _) = u
entryDeadline (Entry _ _ _ _ _ d _ _) = d
entryDone (Entry _ _ _ _ _ _ d _) = d
entryDescription (Entry _ _ _ _ _ _ _ d) = d

renderChange d (Delete u t) = runChildrenWithText [ ("changeClasses", "delete")
                                                  , ("changeDescription", "Deleted")
                                                  , ("changePerson", TE.decodeUtf8 $ uName u)
                                                  , ("changeTime", renderTime t)
                                                  , ("changeDate", renderDate t)
                                                  ]


renderChange d (Change s e c un u t desc) = 
  runChildrenWithText [ ("changeClasses", "change")
                      , ("changeDescription", T.concat ((if BS.length desc /= 0 then ["'", TE.decodeUtf8 desc,"', "] else []) ++ [renderTime s] ++ (if d then [] else ["-",(renderTime e)])))
                      , ("changePerson", TE.decodeUtf8 $ uName u)
                      , ("changeTime", renderTime t)
                      , ("changeDate", renderDate t)
                      ]
                                                    

renderChange d (Cover u t) = runChildrenWithText [ ("changeClasses", "cover")
                                                 , ("changeDescription", "Covered")
                                                 , ("changePerson", TE.decodeUtf8 $ uName u)
                                                 , ("changeTime", renderTime t)
                                                 , ("changeDate", renderDate t)
                                                 ]
                                               
renderEntry (Entry hours units start end changes deadline deadlineDone description) =
  runChildrenWith [ ("hoursWorked", textSplice $ T.pack $ show hours)
                  , ("units", textSplice $ T.pack $ show units)
                  , ("startTime", textSplice $ renderTime start)
                  , ("endTime", textSplice $ renderTime end)
                  , ("shiftDate", textSplice $ renderDateLong start)
                  , ("changes", mapSplices (renderChange deadline) changes)
                  , ("isDeadline", booleanSplice deadline)
                  , ("notDeadline", booleanSplice (not deadline))
                  , ("isDeadlineDone", booleanSplice (deadlineDone || (not deadline)))
                  , ("notDeadlineDone", booleanSplice ((not deadlineDone) || (not deadline)))
                  , ("description", textSplice $ TE.decodeUtf8 description)
                  ]
                  
