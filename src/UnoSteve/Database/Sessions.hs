{-# LANGUAGE QuasiQuotes #-}

module UnoSteve.Database.Sessions
  ( create
  , findByTokenHash
  , setExpiresAt
  , delete
  ) where

import Data.ByteString (ByteString)
import Data.Profunctor (dimap, rmap)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Hasql.Session qualified as Hasql
import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement, resultlessStatement, singletonStatement)

import UnoSteve.Database qualified as Database
import UnoSteve.Domain.Session (Session (..))
import UnoSteve.Domain.User (UserId (..))

-- Operations
create :: Database.Handle -> ByteString -> UserId -> UTCTime -> IO Session
create db tokenHash uid expires =
  Database.runOrThrow db (Hasql.statement (tokenHash, uid, expires) insertSession)

findByTokenHash :: Database.Handle -> ByteString -> IO (Maybe Session)
findByTokenHash db tokenHash = Database.runOrThrow db (Hasql.statement tokenHash selectSession)

setExpiresAt :: Database.Handle -> UUID -> UTCTime -> IO ()
setExpiresAt db sid expires = Database.runOrThrow db (Hasql.statement (sid, expires) updateExpiresAt)

delete :: Database.Handle -> UUID -> IO ()
delete db sid = Database.runOrThrow db (Hasql.statement sid deleteSession)

-- Statements
type SessionRow = (UUID, UUID, UTCTime, UTCTime)

toSession :: SessionRow -> Session
toSession (sid, uuid, created, expires) =
  Session{sessionId = sid, userId = UserId uuid, createdAt = created, expiresAt = expires}

insertSession :: Statement (ByteString, UserId, UTCTime) Session
insertSession =
  dimap
    (\(tokenHash, UserId uuid, expires) -> (tokenHash, uuid, expires))
    toSession
    [singletonStatement|
      insert into sessions (token_hash, user_id, expires_at)
      values ($1 :: bytea, $2 :: uuid, $3 :: timestamptz)
      returning id :: uuid, user_id :: uuid, created_at :: timestamptz, expires_at :: timestamptz
    |]

selectSession :: Statement ByteString (Maybe Session)
selectSession =
  rmap
    (fmap toSession)
    [maybeStatement|
      select id :: uuid, user_id :: uuid, created_at :: timestamptz, expires_at :: timestamptz
      from sessions
      where token_hash = $1 :: bytea
    |]

updateExpiresAt :: Statement (UUID, UTCTime) ()
updateExpiresAt =
  [resultlessStatement|
    update sessions set expires_at = $2 :: timestamptz where id = $1 :: uuid
  |]

deleteSession :: Statement UUID ()
deleteSession =
  [resultlessStatement|
    delete from sessions where id = $1 :: uuid
  |]
