module UnoSteve.Env
  ( Env (..)
  ) where

import UnoSteve.Config (Config)
import UnoSteve.Database qualified as Database
import UnoSteve.Logger qualified as Logger

data Env = Env
  { config :: Config
  , logger :: Logger.Handle
  , database :: Database.Handle
  }
