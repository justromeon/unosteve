module UnoSteve.Env
  ( Env (..)
  ) where

import UnoSteve.Database qualified as Database
import UnoSteve.Logger qualified as Logger
import UnoSteve.PasswordHasher qualified as PasswordHasher
import UnoSteve.Sessions qualified as Sessions

data Env = Env
  { logger :: Logger.Handle
  , database :: Database.Handle
  , passwordHasher :: PasswordHasher.Handle
  , sessions :: Sessions.Handle
  }
