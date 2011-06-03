{-# LANGUAGE OverloadedStrings #-}

module Handlers.Messages where
  
import Snap.Types
import Application

        
messagesH :: Application ()
messagesH = route [ ("/add",            messageAddH)
                  , ("/page/:num",      messagesPageH)
                  , ("/vote/up/:id",    messageVoteUpH)
                  , ("/vote/down/:id",  messageVoteDownH)
                  , ("/flag/:id",       messageFlagH)
                  ]
                  
messageAddH = undefined
messagesPageH = undefined
messageVoteUpH = undefined
messageVoteDownH = undefined
messageFlagH = undefined