{-# LANGUAGE OverloadedStrings #-}

module Render.Messages where

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

renderPages n total = mapSplices (\i -> runChildrenWithText $ do "num" ## T.pack $ show i
                                                                 "class" ## if i == n then "sel" else "") $ take total $ iterate (+1) 1

renderMessages :: [Message] -> Splice AppHandler
renderMessages = mapSplices (\(Message i c u d f r) ->
                              runChildrenWithText $ do "id" ## i
                                                       "message" ## c
                                                       "ups" ## T.pack $ show u
                                                       "downs" ## T.pack $ show d
                                                       "flags" ## T.pack $ show f
                                                       "timestamp" ## T.pack $ formatTime defaultTimeLocale "written %-I:%M%p, %-m.%d.%Y" r
                                                       )
