module UnoSteve.Api
  ( API
  , api
  , app
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Servant

import Network.Wai (Request)
import Servant.Server.Experimental.Auth (AuthHandler)
import UnoSteve.Api.Auth (AuthApi, MeApi, authHandler, authServer, meServer)
import UnoSteve.Database qualified as Database
import UnoSteve.Domain.Session (Session)
import UnoSteve.Env (Env)
import UnoSteve.Env qualified as Env
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
    :<|> "api" :> "v1" :> (AuthApi :<|> MeApi)

api :: Proxy API
api = Proxy

app :: Env -> Application
app env = serveWithContext api (context env) (server env)

context :: Env -> Context '[AuthHandler Request Session]
context env = authHandler env.sessions :. EmptyContext

server :: Env -> Server API
server env =
  healthHandler env.logger
    :<|> readyHandler env.logger env.database
    :<|> versionHandler
    :<|> ( authServer env.passwordHasher env.database env.sessions
             :<|> meServer env.database
         )

healthHandler :: Logger.Handle -> Handler Health
healthHandler logger = do
  liftIO $ Logger.logDebug logger "GET /healthz"
  pure (Health "ok")

readyHandler :: Logger.Handle -> Database.Handle -> Handler Health
readyHandler logger database = do
  result <- liftIO $ try (Database.ping database)
  case result of
    Right () -> pure (Health "ok")
    Left (err :: SomeException) -> do
      liftIO $ Logger.logError logger ("readyz: database is unreachable " <> T.pack (show err))
      throwError err503

versionHandler :: Handler Version
versionHandler = pure (Version "0.1.0")
