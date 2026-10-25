module UnoSteve.Env where

import UnoSteve.Config (Config)
import UnoSteve.Logger qualified as Logger

data Env = Env
  { config :: Config
  , logger :: Logger.Handle
  }
