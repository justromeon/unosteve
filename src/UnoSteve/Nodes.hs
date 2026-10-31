module UnoSteve.Nodes where

import Data.List (unsnoc)
import UnoSteve.Database qualified as Database
import UnoSteve.Database.Nodes qualified as Database.Nodes
import UnoSteve.Domain.Node
  ( Cursor
  , Location (AtRoot, InFolder)
  , Node (kind)
  , NodeKind (Folder)
  , NodeName
  , cursorAfter
  )
import UnoSteve.Domain.Page (PageSize, pageSizeInt)
import UnoSteve.Domain.User (UserId)

data CreateFolderError
  = ParentNotFound
  | NameTaken
  deriving stock (Eq, Show)

-- | A folder at the user's root (no parent) or inside one of their folders.
createFolder ::
  Database.Handle -> UserId -> Location -> NodeName -> IO (Either CreateFolderError Node)
createFolder db owner place folderName = do
  parentExists <- folderExists db owner place
  if not parentExists
    then pure (Left ParentNotFound)
    else do
      result <- Database.Nodes.createFolder db owner place folderName
      case result of
        Left Database.Nodes.NameTaken -> pure (Left NameTaken)
        Right node -> pure (Right node)

data ListError
  = -- | Not a live folder of this user (missing, someone else's, or a file).
    FolderNotFound
  deriving stock (Eq, Show)

data Page = Page
  { items :: [Node]
  , next :: Maybe Cursor
  -- ^ Nothing on the last page.
  }
  deriving stock (Eq, Show)

-- | One page of a folder's children (Nothing: the root), after the cursor if given.
listChildren ::
  Database.Handle -> UserId -> Location -> Maybe Cursor -> PageSize -> IO (Either ListError Page)
listChildren db owner place after size = do
  parentExists <- folderExists db owner place
  if not parentExists
    then pure (Left FolderNotFound)
    else do
      rows <- Database.Nodes.listChildren db owner place after (pageSizeInt size + 1)
      pure $ Right $ toPage (pageSizeInt size) rows

{- | The rule both operations share: the root always exists; otherwise the id must be a
live folder of this user.
-}
folderExists :: Database.Handle -> UserId -> Location -> IO Bool
folderExists _ _ AtRoot = pure True
folderExists db owner (InFolder folderId) =
  maybe False (\node -> node.kind == Folder) <$> Database.Nodes.findById db owner folderId

-- | We asked for one row more than a page holds: if it came back, there's a next page.
toPage :: Int -> [Node] -> Page
toPage size rows = Page{items = pageItems, next = nextCursor}
 where
  (pageItems, extra) = splitAt size rows
  nextCursor = case extra of
    [] -> Nothing
    _ -> cursorAfter . snd <$> unsnoc pageItems
