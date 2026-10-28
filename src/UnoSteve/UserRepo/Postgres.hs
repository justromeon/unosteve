{-# LANGUAGE QuasiQuotes #-}

module UnoSteve.UserRepo.Postgres
  ( new
  , close
  , withHandle
  ) where

import Control.Exception (bracket, throwIO)
import Data.Int (Int64)
import Data.Profunctor (dimap)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Hasql.Session qualified as Session
import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement, singletonStatement)

import UnoSteve.Database qualified as Database
import UnoSteve.Domain.User
  ( Email
  , NewUser (..)
  , PasswordHash (..)
  , User (..)
  , UserId (..)
  , defaultQuotaBytes
  , emailText
  , hashText
  , unsafeEmail
  )
import UnoSteve.UserRepo qualified as UserRepo

new :: Database.Handle -> IO UserRepo.Handle
new db =
  pure
    UserRepo.Handle
      { UserRepo.create = create db
      , UserRepo.findByEmail = findByEmail db
      , UserRepo.findById = findById db
      }

close :: UserRepo.Handle -> IO ()
close _ = pure ()

withHandle :: Database.Handle -> (UserRepo.Handle -> IO a) -> IO a
withHandle db = bracket (new db) close

-- Operations
create :: Database.Handle -> NewUser -> IO (Either UserRepo.CreateError User)
create db newUser = do
  result <- Database.run db (Session.statement newUser insertUser)
  case result of
    Right user -> pure (Right user)
    Left err
      | Database.isUniqueViolation err -> pure (Left UserRepo.EmailTaken)
      | otherwise -> throwIO err

findByEmail :: Database.Handle -> Email -> IO (Maybe User)
findByEmail db emailAdd = Database.runOrThrow db (Session.statement emailAdd selectUserByEmail)

findById :: Database.Handle -> UserId -> IO (Maybe User)
findById db targetId = Database.runOrThrow db (Session.statement targetId selectUserById)

-- Statements
type UserRow = (UUID, Text, Text, Int64, UTCTime)

toUser :: UserRow -> User
toUser (uuid, address, hash, quota, created) =
  User
    { userId = UserId uuid
    , email = unsafeEmail address
    , passwordHash = PasswordHash hash
    , quotaBytes = quota
    , createdAt = created
    }

insertUser :: Statement NewUser User
insertUser =
  dimap
    (\u -> (emailText u.email, hashText u.passwordHash, defaultQuotaBytes))
    toUser
    [singletonStatement|
      insert into users (email, password_hash, quota_bytes)
      values ($1 :: text, $2 :: text, $3 :: int8)
      returning id :: uuid, email :: text, password_hash :: text, quota_bytes :: int8, created_at :: timestamptz
    |]

selectUserByEmail :: Statement Email (Maybe User)
selectUserByEmail =
  dimap
    emailText
    (fmap toUser)
    [maybeStatement|
        select id :: uuid, email :: text, password_hash :: text, quota_bytes :: int8, created_at :: timestamptz
        from users
        where email = $1 :: text
      |]

selectUserById :: Statement UserId (Maybe User)
selectUserById =
  dimap
    (\(UserId uuid) -> uuid)
    (fmap toUser)
    [maybeStatement|
        select id :: uuid, email :: text, password_hash :: text, quota_bytes :: int8, created_at :: timestamptz
        from users
        where id = $1 :: uuid
      |]
