{-# LANGUAGE OverloadedStrings #-}

module Handlers.Messages where
  
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

-- | Module specific imports
import State.Types
import State.Messages
import Forms.Messages
import Render.Messages

        
messagesH :: User -> UserPlace -> AppHandler ()
messagesH u p = route [ ("/add",            messageAddH u p)
                      , ("/page/:num",      messagesPageH u p)
                      , ("/vote/up/:id",    messageVoteUpH u p)
                      , ("/vote/down/:id",  messageVoteDownH u p)
                      , ("/flag/:id",       messageFlagH u p)
                      ]
                  
messageAddH u p = do
  (view, result) <- runForm"add-user-form" messageForm
  case result of
    Nothing ->
      heistLocal (bindDigestiveSplices view) $ renderWS "messages/add_form"
    Just msg' -> do
      addMessage p (TE.encodeUtf8 msg')
      spl <- messagesPageSplices p 1
      heistLocal (bindSplices spl) $ renderWS "messages/add_success"

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
         ,("message-errors", blackHoleSplice)
         ]

messageAct fn u p = do
  mid <- getParam "id"
  case mid of
    Nothing -> pass
    Just id' -> do
      fn p u id'
      spl <- messagesPageSplices p 1
      heistLocal (bindSplices spl) $ renderWS "messages/page"
  
messageVoteUpH = messageAct voteMessageUp
messageVoteDownH = messageAct voteMessageDown
messageFlagH = messageAct flagMessage
