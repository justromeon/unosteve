module UnoSteve.Storage.R2Spec where

import Control.Exception (finally)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.UUID.V4 qualified as UUID
import Network.HTTP.Client
  ( Manager
  , Request (method, requestBody, requestHeaders)
  , RequestBody (RequestBodyLBS)
  , Response (responseBody, responseHeaders, responseStatus)
  , httpLbs
  , parseRequest
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (Status (statusCode), hContentType)
import System.Environment (lookupEnv)
import Test.Hspec (Spec, SpecWith, beforeAll, it, pendingWith, runIO, shouldBe, shouldReturn)
import Test.Support (testName)
import UnoSteve.Clock.System qualified as Clock.System
import UnoSteve.Secret (Secret (Secret))
import UnoSteve.Storage qualified as Storage
import UnoSteve.Storage.R2 qualified as Storage.R2

setup :: IO (Storage.Handle, Manager)
setup = do
  config <- r2Config
  storage <- Storage.R2.new config Clock.System.new
  manager <- newTlsManager
  pure (storage, manager)

-- | Talks to the real R2 dev bucket, so it only runs when asked to (and never in CI).
spec :: Spec
spec = do
  enabled <- runIO (lookupEnv "RUN_R2_TESTS")
  case enabled of
    Just "1" -> beforeAll setup r2Spec
    _ -> it "needs R2" $ pendingWith "set RUN_R2_TESTS=1 to run against the dev bucket (S3_* vars)"

r2Spec :: SpecWith (Storage.Handle, Manager)
r2Spec = do
  it "uploads, inspects, downloads and deletes through presigned URLs" $ \(storage, manager) -> do
    key <- newKey
    flip finally (storage.deleteObject key) $ do
      upload <- storage.presignPut key "text/plain"
      putVia manager upload "text/plain" content `shouldReturn` 200
      storage.headObject key
        `shouldReturn` Just (Storage.ObjectInfo (LBS.length content) (Just "text/plain"))
      download <- storage.presignGet key (testName "hello world.txt")
      response <- getVia manager download
      responseBody response `shouldBe` content
      lookup "Content-Disposition" (responseHeaders response)
        `shouldBe` Just "attachment; filename=\"hello world.txt\"; filename*=UTF-8''hello%20world.txt"
    storage.headObject key `shouldReturn` Nothing

  it "rejects an upload with another Content-Type" $ \(storage, manager) -> do
    key <- newKey
    upload <- storage.presignPut key "text/plain"
    putVia manager upload "image/png" content `shouldReturn` 403
    storage.headObject key `shouldReturn` Nothing

  it "reports a missing object as Nothing and deletes it without error" $ \(storage, _) -> do
    key <- newKey
    storage.headObject key `shouldReturn` Nothing
    storage.deleteObject key
    pure ()

-- Helpers
content :: LBS.ByteString
content = "hello from unosteve"

newKey :: IO Storage.StorageKey
newKey = Storage.StorageKey <$> UUID.nextRandom

-- | What a browser would do with an upload URL; returns the status code.
putVia :: Manager -> Storage.PresignedUrl -> ByteString -> LBS.ByteString -> IO Int
putVia manager presigned mime body = do
  request <- parseRequest (T.unpack presigned.url)
  response <-
    httpLbs
      request{method = "PUT", requestHeaders = [(hContentType, mime)], requestBody = RequestBodyLBS body}
      manager
  pure (statusCode (responseStatus response))

getVia :: Manager -> Storage.PresignedUrl -> IO (Response LBS.ByteString)
getVia manager presigned = do
  request <- parseRequest (T.unpack presigned.url)
  httpLbs request manager

-- | The same S3_* variables the app reads (direnv loads them from .env).
r2Config :: IO Storage.R2.Config
r2Config = do
  let var name = lookupEnv name >>= maybe (fail ("missing " <> name)) (pure . T.pack)
  endpoint <- var "S3_ENDPOINT"
  endpointHost <- either (fail . T.unpack) pure (Storage.R2.parseEndpoint endpoint)
  Storage.R2.Config endpointHost
    <$> var "S3_REGION"
    <*> var "S3_BUCKET"
    <*> var "S3_ACCESS_KEY"
    <*> (Secret <$> var "S3_SECRET_KEY")
    <*> pure 60
    <*> pure 60
