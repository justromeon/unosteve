module UnoSteve.Domain.Session
  ( SessionToken (..)
  , tokenText
  , Session (..)
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)

import UnoSteve.Domain.User (UserId)

-- | What the client holds and sends. Never stored, never logged.
newtype SessionToken = SessionToken Text

instance Show SessionToken where
  show _ = "<session token>"

tokenText :: SessionToken -> Text
tokenText (SessionToken t) = t

data Session = Session
  { sessionId :: UUID
  , userId :: UserId
  , createdAt :: UTCTime
  , expiresAt :: UTCTime
  }
  deriving stock (Eq, Show)
