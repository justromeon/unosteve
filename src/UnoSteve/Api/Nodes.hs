module UnoSteve.Api.Nodes where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON (toJSON), object, (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
import UnoSteve.Database qualified as Database
import UnoSteve.Database.Nodes qualified as Database.Nodes
import UnoSteve.Domain.Node
  ( Cursor
  , FileInfo (mimeType, sizeBytes)
  , Location (AtRoot, InFolder)
  , Node (createdAt, kind, location, name, nodeId, updatedAt)
  , NodeId
  , NodeKind (File, Folder)
  , NodeName
  , cursorText
  , nodeNameText
  , nodeUuid
  , parentUuid
  )
import UnoSteve.Domain.Page (PageSize, defaultPageSize)
import UnoSteve.Domain.Session (Session (userId))
import UnoSteve.Nodes (Page (next))
import UnoSteve.Nodes qualified as Node
import UnoSteve.Nodes qualified as Nodes

-- Only for its instances: `AuthServerData (AuthProtect "session") = Session` lives there.
import UnoSteve.Api.Auth ()

-- | Every route needs a session, so the check sits once in front of the whole group.
type NodesApi =
  AuthProtect "session"
    :> ( "folders" :> ReqBody '[JSON] NewFolder :> PostCreated '[JSON] NodeResponse
           :<|> "folders" :> "root" :> "children" :> Listing
           :<|> "folders" :> Capture "folderId" NodeId :> "children" :> Listing
           :<|> "nodes" :> Capture "nodeId" NodeId :> Get '[JSON] NodeResponse
       )

-- | One page of children: ?limit=1..100 (default 50) &cursor=<nextCursor of the previous page>.
type Listing = QueryParam "limit" PageSize :> QueryParam "cursor" Cursor :> Get '[JSON] PageResponse

-- Request / response bodies

data NewFolder = NewFolder
  { name :: NodeName
  , parentId :: Maybe NodeId
  -- ^ Missing or null: create at the root
  }
  deriving stock (Generic)

instance FromJSON NewFolder

newtype NodeResponse = NodeResponse Node

-- | A tagged union: @kind@ says which fields follow. Folders have no size or MIME type.
instance ToJSON NodeResponse where
  toJSON (NodeResponse node) =
    object $
      [ "id" .= nodeUuid node.nodeId
      , "parentId" .= parentUuid node.location
      , "name" .= nodeNameText node.name
      , "createdAt" .= node.createdAt
      , "updatedAt" .= node.updatedAt
      ]
        <> case node.kind of
          Folder -> ["kind" .= ("folder" :: Text)]
          File file ->
            [ "kind" .= ("file" :: Text)
            , "sizeBytes" .= file.sizeBytes
            , "mimeType" .= file.mimeType
            ]

newtype PageResponse = PageResponse Node.Page

instance ToJSON PageResponse where
  toJSON (PageResponse page) =
    object
      [ "items" .= map NodeResponse page.items
      , "nextCursor" .= fmap cursorText page.next
      ]

-- Handlers
nodesServer :: Database.Handle -> Server NodesApi
nodesServer db session =
  createFolderHandler db session
    :<|> listChildrenHandler db session AtRoot
    :<|> (listChildrenHandler db session . InFolder)
    :<|> getNodeHandler db session

createFolderHandler :: Database.Handle -> Session -> NewFolder -> Handler NodeResponse
createFolderHandler db session body = do
  let place = maybe AtRoot InFolder body.parentId
  result <- liftIO $ Nodes.createFolder db session.userId place body.name
  case result of
    Right node -> pure (NodeResponse node)
    Left Nodes.ParentNotFound -> throwError err404{errBody = "Parent folder not found."}
    Left Nodes.NameTaken -> throwError err409{errBody = "Something with this name already exists in this folder."}

listChildrenHandler ::
  Database.Handle ->
  Session ->
  Location ->
  Maybe PageSize ->
  Maybe Cursor ->
  Handler PageResponse
listChildrenHandler db session place limit after = do
  result <-
    liftIO $ Nodes.listChildren db session.userId place after (fromMaybe defaultPageSize limit)
  case result of
    Right page -> pure (PageResponse page)
    Left Nodes.FolderNotFound -> throwError err404{errBody = "Folder not found."}

getNodeHandler :: Database.Handle -> Session -> NodeId -> Handler NodeResponse
getNodeHandler db session target = do
  found <- liftIO $ Database.Nodes.findById db session.userId target
  maybe (throwError err404{errBody = "Not found."}) (pure . NodeResponse) found
