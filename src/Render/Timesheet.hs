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


data Entry = Entry Double LocalTime LocalTime [Modification] deriving Show -- hours worked, orig. start, orig. end, list of modifications

entryHours (Entry h _ _ _) = h

renderChange (Delete u t) = runChildrenWithText [ ("changeClasses", "delete")
                                                , ("changeDescription", "Deleted")
                                                , ("changePerson", TE.decodeUtf8 $ uName u)
                                                , ("changeTime", renderTime t)
                                                , ("changeDate", renderDate t)
                                                ]


renderChange (Change s e c un u t) = runChildrenWithText [ ("changeClasses", "change")
                                                         , ("changeDescription", T.concat ["To ",(renderTime s),"-",(renderTime e)])
                                                         , ("changePerson", TE.decodeUtf8 $ uName u)
                                                         , ("changeTime", renderTime t)
                                                         , ("changeDate", renderDate t)
                                                         ]
                                                    

renderChange (Cover u t) = runChildrenWithText [ ("changeClasses", "cover")
                                               , ("changeDescription", "Covered")
                                               , ("changePerson", TE.decodeUtf8 $ uName u)
                                               , ("changeTime", renderTime t)
                                               , ("changeDate", renderDate t)
                                               ]
                                               
renderEntry (Entry hours start end changes) =
  runChildrenWith [ ("hoursWorked", textSplice $ T.pack $ show hours)
                  , ("startTime", textSplice $ renderTime start)
                  , ("endTime", textSplice $ renderTime end)
                  , ("shiftDate", textSplice $ renderDateLong start)
                  , ("changes", mapSplices renderChange changes)
                  ]
                  
