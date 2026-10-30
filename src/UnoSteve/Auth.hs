module UnoSteve.Auth
  ( register
  , LoginError (..)
  , login
  ) where

import Control.Exception (evaluate)
import Data.Text (Text)
import UnoSteve.Database qualified as Database
import UnoSteve.Database.Users qualified as Database.Users
import UnoSteve.Domain.Password (Password)
import UnoSteve.Domain.Session (Session, SessionToken)
import UnoSteve.Domain.User (Email, NewUser (NewUser), User (passwordHash, userId))
import UnoSteve.PasswordHasher qualified as PasswordHasher
import UnoSteve.Sessions qualified as Sessions

register ::
  PasswordHasher.Handle ->
  Database.Handle ->
  Email ->
  Password ->
  IO (Either Database.Users.CreateError User)
register hasher db emailAdd password = do
  hashed <- PasswordHasher.hash hasher password
  result <- Database.Users.create db (NewUser emailAdd hashed)
  pure $ result

data LoginError = InvalidCredentials
  deriving (Eq, Show)

login ::
  PasswordHasher.Handle ->
  Database.Handle ->
  Sessions.Handle ->
  Email ->
  Text -> -- see comments on UnoSteve.PasswordHasher.verify
  IO (Either LoginError (SessionToken, Session))
login hasher db sessions emailAdd attempt = do
  found <- Database.Users.findByEmail db emailAdd
  matches <- evaluate $ PasswordHasher.verify hasher attempt (passwordHash <$> found)
  case found of
    Just user | matches -> Right <$> Sessions.start sessions user.userId
    _ -> pure (Left InvalidCredentials)
