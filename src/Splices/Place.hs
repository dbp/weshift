{-# LANGUAGE OverloadedStrings #-}

module Splices.Place where

import Text.Templating.Heist
import Snap.Extension.Heist
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

import State.Types
import Common

renderPlaces :: Monad m => [UserPlace] -> Splice m
renderPlaces = mapSplices (\p -> runChildrenWithText [("name", TE.decodeUtf8 (pName p))
                                                     ,("org", TE.decodeUtf8 (pOrg p))
                                                     ,("root", TE.decodeUtf8 (placeRoot p))
                                                     ])