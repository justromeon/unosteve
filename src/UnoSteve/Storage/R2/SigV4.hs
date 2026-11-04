{- | AWS Signature Version 4, query-string variant: presigned URLs for S3-compatible
storage. amazonka signs our GET/HEAD/DELETE; this signs PUTs, because amazonka never signs
@Content-Length@ and we need R2 to enforce the upload size.
-}
module UnoSteve.Storage.R2.SigV4
  ( Credentials (..)
  , Request (..)
  , presignUrl
  ) where

import Crypto.Hash (SHA256 (SHA256), hashWith)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.ByteArray (ByteArrayAccess, convert)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString (ByteString)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Time (NominalDiffTime, UTCTime, defaultTimeLocale, formatTime)
import Network.HTTP.Types (urlEncode)
import UnoSteve.Secret (Secret (reveal))

data Credentials = Credentials
  { accessKey :: Text
  , secretKey :: Secret
  }

-- | The request a presigned URL allows.
data Request = Request
  { method :: Text
  , host :: Text
  , path :: [Text]
  -- ^ Path segments, unencoded: @[bucket, key]@.
  , headers :: [(Text, Text)]
  -- ^ Signed in addition to @host@: the client must send exactly these values.
  }

-- | A URL valid from @now@ for @validFor@, carrying its signature in the query string.
presignUrl :: Credentials -> Text -> UTCTime -> NominalDiffTime -> Request -> Text
presignUrl credentials region now validFor request =
  "https://"
    <> request.host
    <> canonicalPath
    <> "?"
    <> canonicalQuery
    <> "&X-Amz-Signature="
    <> signature
 where
  timestamp = T.pack (formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" now)
  date = T.take 8 timestamp
  scope = T.intercalate "/" [date, region, "s3", "aws4_request"]

  -- host is always signed; names lowercase, values trimmed, sorted by name.
  signedHeaderList =
    sortOn
      fst
      [(T.toLower name, T.strip value) | (name, value) <- ("host", request.host) : request.headers]
  signedHeaderNames = T.intercalate ";" (map fst signedHeaderList)

  canonicalPath = "/" <> T.intercalate "/" (map uriEncode request.path)
  canonicalQuery =
    T.intercalate
      "&"
      [ uriEncode name <> "=" <> uriEncode value
      | (name, value) <-
          sortOn
            fst
            [ ("X-Amz-Algorithm", algorithm)
            , ("X-Amz-Credential", credentials.accessKey <> "/" <> scope)
            , ("X-Amz-Date", timestamp)
            , ("X-Amz-Expires", T.pack (show (round validFor :: Integer)))
            , ("X-Amz-SignedHeaders", signedHeaderNames)
            ]
      ]

  canonicalRequest =
    T.intercalate
      "\n"
      [ request.method
      , canonicalPath
      , canonicalQuery
      , T.concat [name <> ":" <> value <> "\n" | (name, value) <- signedHeaderList]
      , signedHeaderNames
      , "UNSIGNED-PAYLOAD"
      ]

  stringToSign =
    T.intercalate
      "\n"
      [algorithm, timestamp, scope, hex (hashWith SHA256 (encodeUtf8 canonicalRequest))]

  signingKey =
    hmacSha256
      ( hmacSha256
          (hmacSha256 (hmacSha256 (encodeUtf8 ("AWS4" <> credentials.secretKey.reveal)) date) region)
          "s3"
      )
      "aws4_request"

  signature = hex (hmacSha256 signingKey stringToSign)

algorithm :: Text
algorithm = "AWS4-HMAC-SHA256"

hmacSha256 :: ByteString -> Text -> ByteString
hmacSha256 key message = convert (hmac key (encodeUtf8 message) :: HMAC SHA256)

hex :: (ByteArrayAccess bytes) => bytes -> Text
hex = decodeUtf8Lenient . convertToBase Base16

{- | AWS's URI encoding: everything except @A–Z a–z 0–9 - _ . ~@ becomes %XX (uppercase).
That's exactly http-types' query-string encoding, @urlEncode True@.
-}
uriEncode :: Text -> Text
uriEncode = decodeUtf8Lenient . urlEncode True . encodeUtf8
