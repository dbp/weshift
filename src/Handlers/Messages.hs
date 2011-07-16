{-# LANGUAGE OverloadedStrings #-}

module Handlers.Messages where
  
import Snap.Types
import Text.Templating.Heist
import Snap.Extension.Heist
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T  
import qualified Data.ByteString.Char8 as B8  
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Time.Format
import System.Locale (defaultTimeLocale)
import Application
import Common
import State.Types
import State.Messages

        
messagesH :: User -> UserPlace -> Application ()
messagesH u p = route [ ("/add",            messageAddH u p)
                      , ("/page/:num",      messagesPageH u p)
                      , ("/vote/up/:id",    messageVoteUpH u p)
                      , ("/vote/down/:id",  messageVoteDownH u p)
                      , ("/flag/:id",       messageFlagH u p)
                      ]
                  
messageAddH = undefined

messagesPageH u p = do
  mpage <- getParam "num"
  case mpage >>= maybeRead of
    Nothing -> pass
    Just page -> do
      spl <- messagesPageSplices p page
      heistLocal (bindSplices spl) $ renderWS "messages/page"

messagesPageSplices place n = do
  messages <- getPlaceMessages place n
  numPages <- fmap (fmap (fromIntegral . ceiling . (/ 5) . fromIntegral)) $ getPlaceNumMessages place
  let next = case numPages of
              Nothing -> Nothing -- don't know what to do
              Just num -> if num > n then Just (n+1) else Nothing
  return [("next", textSplice $ maybe "" (T.pack . show) next)
         ,("pages", renderPages n (fromMaybe 0 numPages))
         ,("messages", renderMessages messages)
         ]
  
messageVoteUpH = undefined
messageVoteDownH = undefined
messageFlagH = undefined

renderPages n total = mapSplices (\i -> runChildrenWithText [("num", T.pack $ show i),("class", if i == n then "sel" else "")]) $ take total $ iterate (+1) 1

renderMessages = mapSplices (\(Message i c u d f r) -> 
                              runChildrenWithText [("id", TE.decodeUtf8 i)
                                                  ,("message", TE.decodeUtf8 c)
                                                  ,("ups", T.pack $ show u)
                                                  ,("downs", T.pack $ show d)
                                                  ,("flags", T.pack $ show f)
                                                  ,("timestamp", T.pack $ formatTime defaultTimeLocale "written %-I:%M%p, %-m.%d.%Y" r)
                                                  ])