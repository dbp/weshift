{-# LANGUAGE OverloadedStrings #-}

module Handlers.Messages where
  
import Snap.Types
import Application
import State.Types

        
messagesH :: User -> UserPlace -> Application ()
messagesH u p = route [ ("/add",            messageAddH)
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