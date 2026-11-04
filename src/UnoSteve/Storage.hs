module UnoSteve.Storage where

import Data.Char (isAscii)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Network.HTTP.Types (urlEncode)
import UnoSteve.Domain.Node (NodeName, nodeNameText)

-- | Where an object lives in the bucket: a random UUID, never the user's file name.
newtype StorageKey = StorageKey UUID
  deriving newtype (Eq, Ord)

storageKeyText :: StorageKey -> Text
storageKeyText (StorageKey uuid) = UUID.toText uuid

data PresignedUrl = PresignedUrl
  { url :: Text
  , expiresAt :: UTCTime
  }
  deriving stock (Eq, Show)

data ObjectInfo = ObjectInfo
  { sizeBytes :: Int64
  , contentType :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | Object storage. Signs URLs for clients and inspects or deletes objects; file bytes never pass through.
data Handle = Handle
  { presignPut :: StorageKey -> Text -> Int64 -> IO PresignedUrl
  {- ^ Upload URL accepting only this Content-Type and exactly this many bytes: both are
  signed, so storage refuses anything else before storing it.
  -}
  , presignGet :: StorageKey -> NodeName -> IO PresignedUrl
  , headObject :: StorageKey -> IO (Maybe ObjectInfo)
  , deleteObject :: StorageKey -> IO ()
  }

{- | @attachment@ plus the file name twice (RFC 6266): @filename=@ as an ASCII
fallback for old clients, @filename*=@ as UTF-8 percent-encoded (RFC 8187).
'NodeName' has no control characters, so nothing can break out of the header.
-}
contentDisposition :: NodeName -> Text
contentDisposition fileName =
  "attachment; filename=\"" <> asciiFallback <> "\"; filename*=UTF-8''" <> encoded
 where
  raw = nodeNameText fileName
  encoded = decodeUtf8Lenient $ urlEncode False (encodeUtf8 raw)
  asciiFallback = T.map (\c -> if isAscii c && c /= '"' && c /= '\\' then c else '_') raw
