{-# LANGUAGE OverloadedStrings #-}

import State.Types
import State.Account (buildUser,buildEmail)
import Data.Pool
import Database.HDBC.PostgreSQL
import Database.HDBC
import Data.Maybe (catMaybes)
import Mail
import Utils

import qualified Data.ByteString as BS

import Secrets

withPGDB conn q ps = do r <- quickQuery' conn q ps
                        commit conn
                        return r

getOverdueDeadlines conn = fmap (catMaybes . (map f)) $ withPGDB conn "SELECT S.id, S.user_id, S.place, S.start, S.stop, S.recorded, S.recorder, S.color, S.units, S.deadline, S.deadline_done, S.description, S.has_claims, U.id, U.name, U.active, U.super, U.view, U.token, E.id, E.user_id, E.email, E.confirmed FROM useremails AS E JOIN users AS U ON E.user_id = U.id JOIN shifts_current AS S ON S.user_id = U.id WHERE S.start < now() AND S.start > (now() - interval '1 day') AND E.confirmed = true AND S.deadline = true AND S.deadline_done = false;" []
    where f (si:su:sp:ssta:ssto:sr:srec:sco:sun:sd:sdd:sdesc:shc:ui:un:ua:us:uv:ut:ei:eu:ee:ec:[]) =
            do s <- buildShift (si:su:sp:ssta:ssto:sr:srec:sco:sun:sd:sdd:sdesc:shc:[])
               u <- buildUser (ui:un:ua:us:uv:ut:[]) [] -- we don't care about places in this context
               e <- buildEmail (ei:eu:ee:ec:[])
               return (u,s,e)
          f _ = error "Error in getOverdueDeadlines."



main = do
    c <- connectPostgreSQL ("hostaddr=127.0.0.1 dbname=" ++ dbName ++ " user=" ++ 
                            pgUser ++ " password=" ++ pgPassword)
    dus <- getOverdueDeadlines c
    putStrLn $ show dus
    mapM_ handleOverdue dus


handleOverdue (user, shift, email) =
    mailOverdueDeadline (uName user) (emAddress email) (sDescription shift) (BS.concat [wsFormatDay (sStart shift), ", ", wsFormatTime (sStart shift)])



