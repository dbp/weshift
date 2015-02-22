{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.Pool
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           Snap.Less
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session     hiding (commit)

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess  :: Snaplet SessionManager
    , _db    :: Pool Connection
    , _less  :: LessDirectory
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist


withPGDB :: String -> [SqlValue] -> AppHandler [[SqlValue]]
withPGDB r ps = do c <- use db
                   liftIO $ withResource c (\conn -> do r <- quickQuery' conn r ps
                                                        commit conn
                                                        return r)
withLess :: (LessState -> AppHandler ()) -> AppHandler ()
withLess f = do c <- use less
                ls <- getDirectoryLS c
                f ls

------------------------------------------------------------------------------
type AppHandler = Handler App App
