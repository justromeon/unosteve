{-# LANGUAGE QuasiQuotes #-}

module UnoSteve.Database.Nodes where

import Control.Exception (Exception, throwIO)
import Data.Int (Int64)
import Data.Profunctor (Profunctor (lmap))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Hasql.Session qualified as Session
import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement, singletonStatement, vectorStatement)
import UnoSteve.Database qualified as Database
import UnoSteve.Domain.Node
  ( Cursor (isFile, name, nodeId)
  , FileInfo (FileInfo, mimeType, sizeBytes)
  , Location (AtRoot, InFolder)
  , Node (..)
  , NodeId (NodeId)
  , NodeKind (File, Folder)
  , NodeName
  , nodeNameText
  , unsafeNodeName
  )
import UnoSteve.Domain.User (UserId (UserId))

data CreateError = NameTaken
  deriving stock (Eq, Show)

-- Operations

-- | Insert a folder. Whether the parent is valid is the caller's decision ('UnoSteve.Nodes').
createFolder ::
  Database.Handle -> UserId -> Location -> NodeName -> IO (Either CreateError Node)
createFolder db owner place folderName = do
  result <- Database.run db (Session.statement (owner, place, folderName) insertFolder)
  case result of
    Right row -> Right <$> decodeNode row
    Left err
      | Database.isUniqueViolation err -> pure (Left NameTaken)
      | otherwise -> throwIO err

-- | A live node, only if it belongs to this user.
findById :: Database.Handle -> UserId -> NodeId -> IO (Maybe Node)
findById db owner target =
  Database.runOrThrow db (Session.statement (owner, target) selectNode) >>= traverse decodeNode

-- | Up to @limit@ live children of a folder (Nothing: the root), in listing order.
listChildren :: Database.Handle -> UserId -> Location -> Maybe Cursor -> Int -> IO [Node]
listChildren db owner place after limit = do
  rows <- Database.runOrThrow db $ case after of
    Nothing -> Session.statement (owner, place, limit64) selectChildren
    Just cursor -> Session.statement (owner, place, cursor, limit64) selectChildrenAfter
  traverse decodeNode (Vector.toList rows)
 where
  limit64 = fromIntegral limit :: Int64

-- Rows
type NodeRow = (UUID, UUID, Maybe UUID, Bool, Text, Maybe Int64, Maybe Text, UTCTime, UTCTime)

-- | A row that no node can represent such as file without size.
newtype InvalidNodeRow = InvalidNodeRow UUID
  deriving stock (Show)

instance Exception InvalidNodeRow

decodeNode :: NodeRow -> IO Node
decodeNode (nid, owner, parent, isFolder, rawName, size, mime, created, updated) = do
  nodeKind <- case (isFolder, size, mime) of
    (True, _, _) -> pure Folder
    (False, Just bytes, Just mimeText) -> pure $ File (FileInfo{sizeBytes = bytes, mimeType = mimeText})
    _ -> throwIO (InvalidNodeRow nid)
  pure
    Node
      { nodeId = NodeId nid
      , ownerId = UserId owner
      , location = maybe AtRoot (InFolder . NodeId) parent
      , kind = nodeKind
      , name = unsafeNodeName rawName
      , createdAt = created
      , updatedAt = updated
      }

-- | The nullable @parent_id@ column: NULL at the root.
parentParam :: Location -> Maybe UUID
parentParam = \case
  AtRoot -> Nothing
  InFolder folderId -> Just (nodeUuid folderId)

nodeUuid :: NodeId -> UUID
nodeUuid (NodeId uuid) = uuid

insertFolder :: Statement (UserId, Location, NodeName) NodeRow
insertFolder =
  lmap
    (\(UserId owner, place, folderName) -> (owner, parentParam place, nodeNameText folderName))
    [singletonStatement|
      insert into nodes (owner_id, parent_id, kind, name, status)
      values ($1 :: uuid, $2 :: uuid?, 'folder', $3 :: text, 'ready')
      returning id :: uuid, owner_id :: uuid, parent_id :: uuid?, (kind = 'folder') :: bool,
        name :: text, size_bytes :: int8?, mime_type :: text?,
        created_at :: timestamptz, updated_at :: timestamptz
    |]

selectNode :: Statement (UserId, NodeId) (Maybe NodeRow)
selectNode =
  lmap
    (\(UserId owner, target) -> (owner, nodeUuid target))
    [maybeStatement|
      select id :: uuid, owner_id :: uuid, parent_id :: uuid?, (kind = 'folder') :: bool,
        name :: text, size_bytes :: int8?, mime_type :: text?,
        created_at :: timestamptz, updated_at :: timestamptz
      from nodes
      where owner_id = $1 :: uuid and id = $2 :: uuid and deleted_at is null
    |]

selectChildren :: Statement (UserId, Location, Int64) (Vector NodeRow)
selectChildren =
  lmap
    (\(UserId owner, place, limit) -> (owner, parentParam place, limit))
    [vectorStatement|
      select id :: uuid, owner_id :: uuid, parent_id :: uuid?, (kind = 'folder') :: bool,
        name :: text, size_bytes :: int8?, mime_type :: text?,
        created_at :: timestamptz, updated_at :: timestamptz
      from nodes
      where owner_id = $1 :: uuid
        and parent_id is not distinct from $2 :: uuid?
        and deleted_at is null
      order by kind = 'file', name, id
      limit $3 :: int8
    |]

selectChildrenAfter :: Statement (UserId, Location, Cursor, Int64) (Vector NodeRow)
selectChildrenAfter =
  lmap
    ( \(UserId owner, place, cursor, limit) ->
        (owner, parentParam place, cursor.isFile, nodeNameText cursor.name, nodeUuid cursor.nodeId, limit)
    )
    [vectorStatement|
      select id :: uuid, owner_id :: uuid, parent_id :: uuid?, (kind = 'folder') :: bool,
        name :: text, size_bytes :: int8?, mime_type :: text?,
        created_at :: timestamptz, updated_at :: timestamptz
      from nodes
      where owner_id = $1 :: uuid
        and parent_id is not distinct from $2 :: uuid?
        and deleted_at is null
        and (kind = 'file', name, id) > ($3 :: bool, $4 :: text, $5 :: uuid)
      order by kind = 'file', name, id
      limit $6 :: int8
    |]
