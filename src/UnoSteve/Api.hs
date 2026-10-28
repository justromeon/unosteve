module UnoSteve.Api (API, api, server) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant

import Control.Exception (SomeException, try)
import Data.Text qualified as T
import UnoSteve.Database qualified as Database
import UnoSteve.Env (Env (database, logger))
import UnoSteve.Logger qualified as Logger

newtype Health = Health {status :: Text}
  deriving (Generic)

newtype Version = Version {version :: Text}
  deriving (Generic)

instance ToJSON Health
instance ToJSON Version

type API =
  "healthz" :> Get '[JSON] Health
    :<|> "readyz" :> Get '[JSON] Health
    :<|> "version" :> Get '[JSON] Version

api :: Proxy API
api = Proxy

server :: Env -> Server API
server env =
  healthHandler env
    :<|> readyHandler env
    :<|> versionHandler

healthHandler :: Env -> Handler Health
healthHandler env = do
  liftIO $ Logger.logDebug env.logger "GET /healthz"
  pure (Health "ok")

readyHandler :: Env -> Handler Health
readyHandler env = do
  result <- liftIO $ try (Database.ping env.database)
  case result of
    Right () -> pure (Health "ok")
    Left (err :: SomeException) -> do
      liftIO $ Logger.logError env.logger ("readyz: database is unreachable " <> T.pack (show err))
      throwError err503

versionHandler :: Handler Version
versionHandler = pure (Version "0.1.0")
