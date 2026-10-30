module UnoSteve.ApiSpec (spec) where

import Data.Aeson (FromJSON, decode, encode, object, (.=))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Types (Method, hAuthorization, hContentType, methodGet, methodPost)
import Network.Wai (Application)
import Network.Wai.Test (SResponse, simpleBody)
import Test.Hspec
import Test.Hspec.Wai (ResponseMatcher (..), WaiSession, request, shouldRespondWith, (<:>))

import Test.Support (postgresSpec, sessionConfig, testHasherConfig)
import UnoSteve.Api (app)
import UnoSteve.Clock.System qualified as Clock.System
import UnoSteve.Database qualified as Database
import UnoSteve.Env (Env (Env))
import UnoSteve.Logger.Noop qualified as Logger.Noop
import UnoSteve.PasswordHasher qualified as PasswordHasher
import UnoSteve.Sessions qualified as Sessions

spec :: Spec
spec = postgresSpec (beforeWith setup apiSpec)

-- | The real application over the test database: cheap argon2, the real clock, no logs.
setup :: Database.Handle -> IO ((), Application)
setup db = do
  hasher <- PasswordHasher.new testHasherConfig
  let sessions = Sessions.new sessionConfig db Clock.System.new
  pure ((), app (Env Logger.Noop.new db hasher sessions))

apiSpec :: SpecWith ((), Application)
apiSpec = do
  it "registers once, then rejects the same email" $ do
    register justin `shouldRespondWith` 201
    register justin `shouldRespondWith` 409

  it "rejects a short password with a 400" $
    register (credentials "justin@example.com" "short") `shouldRespondWith` 400

  it "answers a wrong password with a 401 and a Bearer challenge" $ do
    _ <- register justin
    postJson "/api/v1/auth/login" (credentials "justin@example.com" "wrong horse battery")
      `shouldRespondWith` 401{matchHeaders = ["WWW-Authenticate" <:> "Bearer"]}

  it "rejects /me without a token" $
    request methodGet "/api/v1/me" [] ""
      `shouldRespondWith` 401{matchHeaders = ["WWW-Authenticate" <:> "Bearer"]}

  it "logs in, reaches /me, and logs out" $ do
    _ <- register justin
    bearer <- logIn justin
    authed methodGet "/api/v1/me" bearer `shouldRespondWith` 200
    authed methodPost "/api/v1/auth/logout" bearer `shouldRespondWith` 204
    authed methodGet "/api/v1/me" bearer `shouldRespondWith` 401

  it "logs out everywhere, without touching other users" $ do
    _ <- register justin
    _ <- register other
    laptop <- logIn justin
    phone <- logIn justin
    othersToken <- logIn other
    authed methodPost "/api/v1/auth/logout-all" laptop `shouldRespondWith` 204
    authed methodGet "/api/v1/me" laptop `shouldRespondWith` 401
    authed methodGet "/api/v1/me" phone `shouldRespondWith` 401
    authed methodGet "/api/v1/me" othersToken `shouldRespondWith` 200

  it "sends the security headers" $
    request methodGet "/healthz" [] ""
      `shouldRespondWith` 200
        { matchHeaders = ["X-Content-Type-Options" <:> "nosniff", "Cache-Control" <:> "no-store"]
        }
 where
  justin = credentials "justin@example.com" "correct horse battery"
  other = credentials "other@example.com" "another long password"

-- Helpers -------------------------------------------------------------------

postJson :: ByteString -> LBS.ByteString -> WaiSession st SResponse
postJson path = request methodPost path [(hContentType, "application/json")]

register :: LBS.ByteString -> WaiSession st SResponse
register = postJson "/api/v1/auth/register"

-- | A request carrying a bearer token.
authed :: Method -> ByteString -> ByteString -> WaiSession st SResponse
authed method path bearer = request method path [(hAuthorization, "Bearer " <> bearer)] ""

credentials :: Text -> Text -> LBS.ByteString
credentials address pass = encode (object ["email" .= address, "password" .= pass])

-- | Just the part of the login response the tests need.
newtype LoginBody = LoginBody {token :: Text}
  deriving stock (Generic)

instance FromJSON LoginBody

-- | Log in with these credentials (already registered); returns a bearer token.
logIn :: LBS.ByteString -> WaiSession st ByteString
logIn creds = do
  response <- postJson "/api/v1/auth/login" creds
  Just (LoginBody issued) <- pure (decode (simpleBody response))
  pure (encodeUtf8 issued)
