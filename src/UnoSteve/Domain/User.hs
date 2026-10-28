module UnoSteve.Domain.User
  ( User (..)
  , NewUser (..)
  , UserId (..)
  , Email
  , mkEmail
  , emailText
  , unsafeEmail
  , PasswordHash (..)
  , hashText
  , defaultQuotaBytes
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.UUID (UUID)

newtype UserId = UserId UUID
  deriving newtype (Eq, Ord, Show)

newtype Email = Email Text
  deriving newtype (Eq, Ord, Show)

mkEmail :: Text -> Either Text Email
mkEmail raw
  | T.length normalised > 254 = Left "email must be at most 254 characters"
  | [local, domain] <- T.splitOn "@" normalised
  , not (T.null local)
  , not (T.null domain) =
      Right (Email normalised)
  | otherwise = Left ("invalid email address: " <> raw)
 where
  normalised = T.toLower (T.strip raw)

emailText :: Email -> Text
emailText (Email t) = t

unsafeEmail :: Text -> Email
unsafeEmail = Email

newtype PasswordHash = PasswordHash Text
  deriving newtype (Eq)

instance Show PasswordHash where
  show _ = "<password hash>"

hashText :: PasswordHash -> Text
hashText (PasswordHash t) = t

defaultQuotaBytes :: Int64
defaultQuotaBytes = 1024 * 1024 * 1024 -- 1 GiB

data User = User
  { userId :: UserId
  , email :: Email
  , passwordHash :: PasswordHash
  , quotaBytes :: Int64
  , createdAt :: UTCTime
  }
  deriving stock (Eq, Show)

data NewUser = NewUser
  { email :: Email
  , passwordHash :: PasswordHash
  }
