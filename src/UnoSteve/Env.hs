module UnoSteve.Env
  ( Env (..)
  ) where

import UnoSteve.Database qualified as Database
import UnoSteve.Logger qualified as Logger
import UnoSteve.PasswordHasher qualified as PasswordHasher
import UnoSteve.Sessions qualified as Sessions
import UnoSteve.Storage qualified as Storage

data Env = Env
  { logger :: Logger.Handle
  , database :: Database.Handle
  , passwordHasher :: PasswordHasher.Handle
  , sessions :: Sessions.Handle
  , storage :: Storage.Handle
  }
