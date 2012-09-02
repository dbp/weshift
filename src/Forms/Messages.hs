{-# LANGUAGE OverloadedStrings #-}

module Forms.Messages where

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

messageForm = "message" .: maxOneForty (nonEmpty (text Nothing))
  where maxOneForty = check "Message cannot be over 140 Characters" $ \m -> T.length m < 141
