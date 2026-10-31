module UnoSteve.Domain.Node
  ( NodeId (..)
  , NodeKind (..)
  , FileInfo (..)
  , NodeName
  , mkNodeName
  , nodeNameText
  , unsafeNodeName
  , Node (..)
  , Cursor (..)
  , cursorAfter
  , cursorText
  , parseCursor
  ) where

import Data.Aeson (FromJSON (parseJSON), withText)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (Bifunctor (first))
import Data.ByteArray.Encoding (Base (Base64URLUnpadded), convertFromBase, convertToBase)
import Data.ByteString (ByteString)
import Data.ByteString qualified as LBS
import Data.Char (isControl)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import UnoSteve.Domain.User (UserId)
import Web.HttpApiData (FromHttpApiData (parseUrlPiece))

newtype NodeId = NodeId UUID
  deriving newtype (Eq, Ord, Show, FromJSON, FromHttpApiData)

data NodeKind
  = Folder
  | File FileInfo
  deriving stock (Eq, Show)

data FileInfo = FileInfo
  { sizeBytes :: Int64
  , mimeType :: Text
  }
  deriving stock (Eq, Ord, Show)

newtype NodeName = NodeName Text
  deriving newtype (Eq, Ord, Show)

instance FromJSON NodeName where
  parseJSON = withText "NodeName" (either (fail . T.unpack) pure . mkNodeName)

mkNodeName :: Text -> Either Text NodeName
mkNodeName raw
  | T.null raw = Left "name must not be empty"
  | T.length raw > 255 = Left "name must be at most 255 characters"
  | raw `elem` [".", ".."] = Left "name must not be . or .."
  | T.elem '/' raw = Left "name must not contain /"
  | T.any isControl raw = Left "name must not contain control characters"
  | otherwise = Right (NodeName raw)

nodeNameText :: NodeName -> Text
nodeNameText (NodeName t) = t

-- | For names read back from the database, which already passed 'mkNodeName'.
unsafeNodeName :: Text -> NodeName
unsafeNodeName = NodeName

data Node = Node
  { nodeId :: NodeId
  , ownerId :: UserId
  , parentId :: Maybe NodeId
  -- ^ Nothing: the node sits at its owner's root
  , kind :: NodeKind
  , name :: NodeName
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Show)

-- \| Where a page of children ended, in listing order: folders first, then name, then id.
--  Clients get it as opaque text ('cursorText') and send it back unchanged.
--
data Cursor = Cursor
  { isFile :: Bool
  , name :: NodeName
  , nodeId :: NodeId
  }
  deriving stock (Eq, Show)

instance FromHttpApiData Cursor where
  parseUrlPiece = parseCursor

-- | The cursor that continues a listing right after this node.
cursorAfter :: Node -> Cursor
cursorAfter node = Cursor{isFile = isFileKind, name = node.name, nodeId = node.nodeId}
 where
  isFileKind = case node.kind of
    File _ -> True
    Folder -> False

-- | base64url(JSON [isFile, name, id]): opaque to clients, not secret.
cursorText :: Cursor -> Text
cursorText cursor =
  decodeUtf8Lenient . convertToBase Base64URLUnpadded . LBS.toStrict $
    Aeson.encode (cursor.isFile, nodeNameText cursor.name, uuid)
 where
  NodeId uuid = cursor.nodeId

parseCursor :: Text -> Either Text Cursor
parseCursor raw = do
  bytes <-
    first
      (const invalid)
      (convertFromBase Base64URLUnpadded (encodeUtf8 raw) :: Either String ByteString)
  (file, rawName, uuid) <- maybe (Left invalid) Right (Aeson.decodeStrict bytes)
  cursorName <- mkNodeName rawName
  pure Cursor{isFile = file, name = cursorName, nodeId = NodeId uuid}
 where
  invalid = "invalid cursor"
