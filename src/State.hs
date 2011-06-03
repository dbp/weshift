{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module State where

import qualified  Snap.Auth as A
import            Data.Bson hiding (lookup)
import qualified  Data.Bson as B
import            Control.Monad
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Typeable
import            Data.Maybe (catMaybes, listToMaybe)
import            Data.List.Split (splitOn)
import            Control.Monad
import            Control.Monad.Trans
import            Control.Monad.Reader

-- stolen from cgi:
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
