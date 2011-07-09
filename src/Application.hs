{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, MultiParamTypeClasses, PackageImports #-}

{-

This module defines our application's monad and any application-specific
information it requires.

-}

module Application
  ( Application
  , applicationInitializer
  , withPGDB
  ) where

import            Snap.Extension
import            Snap.Extension.Heist.Impl
import            Snap.Auth
import            Snap.Extension.Session.CookieSession
import            Database.HDBC.PostgreSQL
import            Database.HDBC
import  "mtl"     Control.Monad.Trans (liftIO)
import            Data.Pool
import  "mtl"     Control.Monad.Reader (asks)

type Application = SnapExtend ApplicationState

data ApplicationState = ApplicationState
    { templateState :: HeistState Application
    , cookieState   :: CookieSessionState
    , pgPool        :: Pool Connection
    }

instance HasHeistState Application ApplicationState where
    getHeistState     = templateState
    setHeistState s a = a { templateState = s }

instance HasCookieSessionState ApplicationState where
    getCookieSessionState = cookieState
 
instance MonadAuth Application

withPGDB r ps = do c <- asks pgPool
                   liftIO $ withResource c (\conn -> do r <- quickQuery' conn r ps
                                                        commit conn
                                                        return r)

applicationInitializer :: Initializer ApplicationState
applicationInitializer = do
    heist  <- heistInitializer "resources/templates" id
    cookie <- cookieSessionStateInitializer $ defCookieSessionState
              { csKeyPath = "config/site-key.txt" 
              , csCookieName = "weshift-session" }
    pgconn <- liftIO $ createPool
                        (connectPostgreSQL "hostaddr=127.0.0.1 dbname=postgres user=postgres password=pass") 
                        disconnect 
                        1
                        10
                        5
    return $ ApplicationState heist cookie pgconn
