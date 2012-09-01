{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Data.Lens.Template
import Data.Lens.Common
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Session hiding (commit)
import Database.HDBC.PostgreSQL
import Database.HDBC
import Data.Pool
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _db :: Pool Connection
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist


withPGDB :: String -> [SqlValue] -> AppHandler [[SqlValue]]
withPGDB r ps = do c <- get
                   liftIO $ withResource (getL db c) (\conn -> do r <- quickQuery' conn r ps
                                                                  commit conn
                                                                  return r)


------------------------------------------------------------------------------
type AppHandler = Handler App App


