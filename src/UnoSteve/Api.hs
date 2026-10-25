module UnoSteve.Api (API, api, server) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant

import UnoSteve.Env (Env (logger))
import UnoSteve.Logger qualified as Logger

newtype Health = Health { status :: Text }
  deriving Generic

newtype Version = Version { version :: Text }
  deriving Generic

instance ToJSON Health
instance ToJSON Version

type API
  =    "healthz" :> Get '[JSON] Health
  :<|> "version" :> Get '[JSON] Version

api :: Proxy API
api = Proxy

server :: Env -> Server API
server env = healthHandler env :<|> versionHandler

healthHandler :: Env -> Handler Health
healthHandler env = do
  liftIO $ Logger.logDebug env.logger "GET /healthz"
  pure (Health "ok")

versionHandler :: Handler Version
versionHandler = pure (Version "0.1.0")
