{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module UnoSteve.Api.Auth where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON (toJSON), object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient)
import GHC.Generics (Generic)
import Network.HTTP.Types (hAuthorization)
import Network.Wai (Request (requestHeaders))
import Network.Wai.Middleware.HttpAuth (extractBearerAuth)
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import UnoSteve.Auth qualified as Auth
import UnoSteve.Database qualified as Database
import UnoSteve.Database.Users qualified as Database.Users
import UnoSteve.Domain.Password (Password)
import UnoSteve.Domain.Session (Session (expiresAt, userId), SessionToken (SessionToken), tokenText)
import UnoSteve.Domain.User
  ( Email
  , User (createdAt, email, quotaBytes, userId)
  , UserId (UserId)
  , emailText
  )
import UnoSteve.PasswordHasher qualified as PasswordHasher
import UnoSteve.Sessions qualified as Sessions

type instance AuthServerData (AuthProtect "session") = Session

type AuthApi =
  "auth"
    :> ( "register" :> ReqBody '[JSON] Registration :> PostCreated '[JSON] UserResponse
           :<|> "login" :> ReqBody '[JSON] Credentials :> Post '[JSON] LoginResponse
           :<|> "logout" :> AuthProtect "session" :> PostNoContent
           :<|> "logout-all" :> AuthProtect "session" :> PostNoContent
       )

type MeApi = "me" :> AuthProtect "session" :> Get '[JSON] UserResponse

-- response / request bodies
data Registration = Registration
  { email :: Email
  , password :: Password
  }
  deriving stock (Generic)

instance FromJSON Registration

data Credentials = Credentials
  { email :: Email
  , password :: Text
  }
  deriving (Generic)

instance FromJSON Credentials

-- What clients can only see
newtype UserResponse = UserResponse User

instance ToJSON UserResponse where
  toJSON (UserResponse user) =
    object
      [ "id" .= uuid
      , "email" .= emailText user.email
      , "quotaBytes" .= user.quotaBytes
      , "createdAt" .= user.createdAt
      ]
   where
    UserId uuid = user.userId

data LoginResponse = LoginResponse SessionToken Session

instance ToJSON LoginResponse where
  toJSON (LoginResponse token session) =
    object
      [ "token" .= tokenText token
      , "expiresAt" .= session.expiresAt
      ]

-- Handlers
authServer :: PasswordHasher.Handle -> Database.Handle -> Sessions.Handle -> Server AuthApi
authServer hasher db sessions =
  registerHandler hasher db
    :<|> loginHandler hasher db sessions
    :<|> logoutHandler sessions
    :<|> logoutAllHandler sessions

registerHandler :: PasswordHasher.Handle -> Database.Handle -> Registration -> Handler UserResponse
registerHandler hasher db reg = do
  result <- liftIO $ Auth.register hasher db reg.email reg.password
  case result of
    Right user -> pure (UserResponse user)
    Left Database.Users.EmailTaken -> throwError err409{errBody = "An account with this email already exists."}

loginHandler ::
  PasswordHasher.Handle ->
  Database.Handle ->
  Sessions.Handle ->
  Credentials ->
  Handler LoginResponse
loginHandler hasher db s cred = do
  result <- liftIO $ Auth.login hasher db s cred.email cred.password
  case result of
    Right (token, session) -> pure (LoginResponse token session)
    Left Auth.InvalidCredentials -> throwError (unauthorized "Invalid email or password")

logoutHandler :: Sessions.Handle -> Session -> Handler NoContent
logoutHandler sessions session = NoContent <$ liftIO (Sessions.end sessions session)

logoutAllHandler :: Sessions.Handle -> Session -> Handler NoContent
logoutAllHandler sessions session = NoContent <$ liftIO (Sessions.endAll sessions session.userId)

meServer :: Database.Handle -> Server MeApi
meServer db session = do
  found <- liftIO $ Database.Users.findById db session.userId
  maybe (throwError $ unauthorized "Account no longer exists.") (pure . UserResponse) found

-- Auth helpers

-- | 401 with the header RFC 6750 asks for, telling clients to send a Bearer token.
unauthorized :: LBS.ByteString -> ServerError
unauthorized msg = err401{errBody = msg, errHeaders = [("WWW-Authenticate", "Bearer")]}

-- | Runs before every `AuthProtect "session"` route: no valid session, no handler.
authHandler :: Sessions.Handle -> AuthHandler Request Session
authHandler sessions = mkAuthHandler $ \req ->
  case lookup hAuthorization (requestHeaders req) >>= extractBearerAuth of
    Nothing -> throwError (unauthorized "Missing bearer token.")
    Just raw -> do
      found <- liftIO $ Sessions.authenticate sessions (SessionToken $ decodeUtf8Lenient raw)
      maybe (throwError $ unauthorized "Invalid or expired session.") pure found
