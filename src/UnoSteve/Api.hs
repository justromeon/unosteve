module UnoSteve.Api (API, api, server) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant

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

server :: Server API
server = healthHandler :<|> versionHandler

healthHandler :: Handler Health
healthHandler = pure (Health "ok")

versionHandler :: Handler Version
versionHandler = pure (Version "0.1.0")
