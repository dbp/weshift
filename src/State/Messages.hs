{-# LANGUAGE OverloadedStrings #-}

module State.Messages where


import            Control.Monad
import            Control.Monad.Trans
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            Data.Maybe (catMaybes, mapMaybe, listToMaybe)
import            Database.HDBC
import            Data.Time.LocalTime

import Application
import State.Types
import Auth

-- | the messages are sorted by a falloff algorithm - the votes down are subtracted from the votes up - this is turned to zero if it is negative (votes down alone are not enough to get it to shoot to the end of the list, that's what an innapropriate flag is for). then this is divided by the age of the message. So if something gets a lot of votes, it will stay at the top for a while, but after a while, something that has less votes, but is newer, will replace it. This is pretty standard for this kind of thing (though not sure if doing it purely in SQL is...)
  -- PAGE is BASE 1! this is to match the UI
getPlaceMessages :: UserPlace -> Int -> Application [Message]
getPlaceMessages place page = fmap (mapMaybe buildMessage) $ withPGDB "SELECT * FROM (SELECT id, contents, (SELECT count(message) FROM message_ups WHERE message = id) as ups, (SELECT count(message) FROM message_downs WHERE message = id) as downs, (SELECT count(message) FROM message_flags WHERE message = id) as flags, created FROM messages WHERE place = ? AND parent IS NULL) AS s WHERE s.flags < 4 ORDER BY ((abs(s.ups - s.downs) + (s.ups - s.downs) + 2) / (2 * (extract(epoch from now() - s.created)))) DESC OFFSET ? LIMIT ?;" [toSql (pId place), toSql ((page-1) * 5), toSql (5::Int)]

-- comment from IRC: perhaps do select ... from messages m left join (select message, count(*) from message_ups group by message) u on (u.message=m.id) left join (select message, count(*) from message_downs group by message) d on d.message=m.id ..
-- or  join messages against each of the other tables separately, group the results of the joins, and then join the whole lot of that together

-- Or: 
-- WITH 
-- interesting_messages AS (SELECT id AS message_id FROM messages WHERE place = 1 AND parent IS NULL)
-- ups AS (SELECT message_id, count(*) AS cnt FROM message_ups JOIN interesting_messages USING (message_id) GROUP BY message),
-- downs AS (SELECT messaage_id, count(*) AS cnt FROM message_downs JOIN interesting_messages USING (message_id) GROUP BY message),
-- flags AS (SELECT message_id, count(*) AS cnt FROM messagE_flags JOIN interesting_messages USING (message_id) GROUP BY message)
-- SELECT id, place, parent, contents, ups.cnt, downs.cnt, flags.cnt, created
-- FROM interesting_messages m
-- LEFT OUTER JOIN ups ON (m.id = ups.message_id)
-- LEFT OUTER JOIN downs ON (m.id = downs.message_id)
-- LEFT OUTER JOIN flags ON (m.id = flags.message_id)

getPlaceNumMessages :: UserPlace -> Application (Maybe Int)
getPlaceNumMessages place = fmap ((fmap fromSql).(>>= listToMaybe).listToMaybe) $ withPGDB "SELECT count(id) FROM (SELECT id, (SELECT count(message) FROM message_flags WHERE message = id) as flags FROM messages WHERE place = ? AND parent IS NULL) AS s WHERE s.flags < 4;" [toSql (pId place)]

addMessage :: UserPlace -> BS.ByteString -> Application Bool
addMessage place msg = fmap (not.null) $ withPGDB "INSERT INTO messages (contents, place) VALUES (?, ?) RETURNING id;" [toSql msg, toSql (pId place)]

voteMessageUp :: UserPlace -> User -> BS.ByteString -> Application Bool
voteMessageUp place user msgid = fmap (not.null) $ withPGDB "INSERT INTO message_ups (message, user_hash) (SELECT ?, encode(digest(?, 'sha1'),'hex') WHERE NOT EXISTS (SELECT message FROM message_ups WHERE message=? AND user_hash=encode(digest(?, 'sha1'), 'hex'))) RETURNING message;" [toSql msgid, toSql ident, toSql msgid, toSql ident]
  where ident = BS.concat [msgid,"_",(uId user)]
  
voteMessageDown :: UserPlace -> User -> BS.ByteString -> Application Bool
voteMessageDown place user msgid = fmap (not.null) $ withPGDB "INSERT INTO message_downs (message, user_hash) (SELECT ?, encode(digest(?, 'sha1'),'hex') WHERE NOT EXISTS (SELECT message FROM message_downs WHERE message=? AND user_hash=encode(digest(?, 'sha1'), 'hex'))) RETURNING message;" [toSql msgid, toSql ident, toSql msgid, toSql ident]
  where ident = BS.concat [msgid,"_",(uId user)]

flagMessage :: UserPlace -> User -> BS.ByteString -> Application Bool
flagMessage place user msgid = fmap (not.null) $ withPGDB "INSERT INTO message_flags (message, user_hash) (SELECT ?, encode(digest(?, 'sha1'),'hex') WHERE NOT EXISTS (SELECT message FROM message_flags WHERE message=? AND user_hash=encode(digest(?, 'sha1'), 'hex'))) RETURNING message;" [toSql msgid, toSql ident, toSql msgid, toSql ident]
  where ident = BS.concat [msgid,"_",(uId user)]
