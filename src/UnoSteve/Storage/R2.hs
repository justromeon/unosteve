module UnoSteve.Storage.R2 where

import Amazonka qualified as AWS
import Amazonka.Auth qualified as AWS
import Amazonka.S3 qualified as S3
import Amazonka.S3.GetObject qualified as S3.GetObject
import Amazonka.S3.HeadObject qualified as S3.HeadObject
import Amazonka.S3.PutObject qualified as S3.PutObject
import Control.Exception (throwIO)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Time (NominalDiffTime, addUTCTime)
import Data.UUID qualified as UUID
import Network.HTTP.Types (Status (statusCode))
import Network.URI
  ( URI (uriAuthority, uriFragment, uriPath, uriQuery, uriScheme)
  , URIAuth (uriPort, uriRegName, uriUserInfo)
  , parseAbsoluteURI
  )
import UnoSteve.Clock qualified as Clock
import UnoSteve.Secret (Secret (reveal))
import UnoSteve.Storage (StorageKey (..))
import UnoSteve.Storage qualified as Storage

data Config = Config
  { host :: Host
  , region :: Text
  -- ^ Always @auto@ on R2.
  , bucket :: Text
  , accessKey :: Text
  , secretKey :: Secret
  , uploadExpiry :: NominalDiffTime
  , downloadExpiry :: NominalDiffTime
  }
  deriving stock (Show)

newtype Host = Host Text
  deriving newtype (Show)

-- \| @https://<host>@ (optionally with a trailing slash) down to the host amazonka wants.
parseEndpoint :: Text -> Either Text Host
parseEndpoint raw = case parseAbsoluteURI (T.unpack raw) of
  Just uri
    | uriScheme uri == "https:"
    , Just authority <- uriAuthority uri
    , null (uriUserInfo authority) && null (uriPort authority)
    , uriPath uri `elem` ["", "/"]
    , null (uriQuery uri) && null (uriFragment uri) ->
        Right (Host $ T.pack $ uriRegName authority)
  _ -> Left ("S3_ENDPOINT must be https://<host> with no path or port, got: " <> raw)

{- | Builds the amazonka environment (it holds an HTTP connection manager) and checks the
 credentials with one HEAD, so a bad key fails at startup, not on the first upload.
 Nothing to release: the connection manager is garbage-collected.
-}
new :: Config -> Clock.Handle -> IO Storage.Handle
new config clock = do
  env <- newEnv config
  let objectKey key = S3.ObjectKey (Storage.storageKeyText key)
      bucketName = S3.BucketName config.bucket

      presign :: (AWS.AWSRequest a) => NominalDiffTime -> a -> IO Storage.PresignedUrl
      presign validFor request = do
        now <- clock.now
        raw <- AWS.presignURL env now (toSeconds validFor) request
        pure Storage.PresignedUrl{url = decodeUtf8Lenient raw, expiresAt = addUTCTime validFor now}

      handle =
        Storage.Handle
          { Storage.presignPut = \key mime size ->
              presign config.uploadExpiry $
                (S3.newPutObject bucketName (objectKey key) (AWS.toBody ("" :: ByteString)))
                  { S3.PutObject.contentType = Just mime
                  , S3.PutObject.contentLength = Just (toInteger size)
                  }
          , Storage.presignGet = \key fileName ->
              presign config.downloadExpiry $
                (S3.newGetObject bucketName (objectKey key))
                  { S3.GetObject.responseContentDisposition = Just (Storage.contentDisposition fileName)
                  }
          , Storage.headObject = \key -> do
              result <- AWS.runResourceT (AWS.sendEither env (S3.newHeadObject bucketName (objectKey key)))
              case result of
                Right response -> pure (Just (toObjectInfo response))
                Left (AWS.ServiceError err) | statusCode err.status == 404 -> pure Nothing
                Left err -> throwIO err
          , Storage.deleteObject =
              void . AWS.runResourceT . AWS.send env . S3.newDeleteObject bucketName . objectKey
          }
  _ <- handle.headObject (StorageKey UUID.nil)
  pure handle

newEnv :: Config -> IO AWS.Env
newEnv config = do
  noAuth <- AWS.newEnvNoAuth
  pure
    . AWS.configureService r2
    . AWS.fromKeys
      (AWS.AccessKey (encodeUtf8 config.accessKey))
      (AWS.SecretKey (encodeUtf8 config.secretKey.reveal))
    $ noAuth{AWS.region = AWS.Region' config.region}
 where
  Host hostName = config.host
  -- R2 speaks the S3 API at the account's own host. Path-style URLs (host/bucket/key)
  -- keep the bucket name out of the hostname.
  r2 =
    (AWS.setEndpoint True (encodeUtf8 hostName) 443 S3.defaultService)
      { AWS.s3AddressingStyle = AWS.S3AddressingStylePath
      }

toObjectInfo :: S3.HeadObjectResponse -> Storage.ObjectInfo
toObjectInfo response =
  Storage.ObjectInfo
    { Storage.sizeBytes = maybe 0 fromInteger response.contentLength
    , Storage.contentType = response.contentType
    }

toSeconds :: NominalDiffTime -> AWS.Seconds
toSeconds = fromInteger . round
