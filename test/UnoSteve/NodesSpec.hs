module UnoSteve.NodesSpec where

import Data.Foldable (for_)
import Data.UUID (nil)
import Test.Hspec (Spec, SpecWith, beforeWith, it, shouldBe, shouldReturn)
import Test.Support (postgresSpec, testFolder, testName, testPageSize, testUser)
import UnoSteve.Database qualified as Database
import UnoSteve.Database.Users qualified as Database.Users
import UnoSteve.Domain.Node
  ( Cursor
  , Location (AtRoot, InFolder)
  , Node (location, name, nodeId)
  , NodeId (NodeId)
  , NodeName
  )
import UnoSteve.Domain.User (User (userId), UserId)
import UnoSteve.Nodes (Page (items))
import UnoSteve.Nodes qualified as Nodes

spec :: Spec
spec = postgresSpec (beforeWith setup nodesSpec)

setup :: Database.Handle -> IO (Database.Handle, UserId, UserId)
setup db = do
  Right justin <- Database.Users.create db (testUser "justin@example.com")
  Right other <- Database.Users.create db (testUser "other@example.com")
  pure (db, justin.userId, other.userId)

nodesSpec :: SpecWith (Database.Handle, UserId, UserId)
nodesSpec = do
  it "lists the root sorted by name, on one page" $ \(db, justin, _) -> do
    for_ ["b", "a", "c"] (testFolder db justin AtRoot)
    Right page <- Nodes.listChildren db justin AtRoot Nothing (testPageSize 10)
    map name page.items `shouldBe` map testName ["a", "b", "c"]
    page.next `shouldBe` Nothing

  it "walks every page with the cursor" $ \(db, justin, _) -> do
    for_ ["e", "c", "a", "d", "b"] (testFolder db justin AtRoot)
    allPages db justin Nothing `shouldReturn` map (map testName) [["a", "b"], ["c", "d"], ["e"]]

  it "ends without an empty page when the last page is exactly full" $ \(db, justin, _) -> do
    for_ ["a", "b", "c", "d"] (testFolder db justin AtRoot)
    allPages db justin Nothing `shouldReturn` map (map testName) [["a", "b"], ["c", "d"]]

  it "lists only the folder's own children" $ \(db, justin, _) -> do
    photos <- testFolder db justin AtRoot "Photos"
    _ <- testFolder db justin (InFolder photos.nodeId) "Trip"
    Right root <- Nodes.listChildren db justin AtRoot Nothing (testPageSize 10)
    Right inside <- Nodes.listChildren db justin (InFolder photos.nodeId) Nothing (testPageSize 10)
    map name root.items `shouldBe` [testName "Photos"]
    map name inside.items `shouldBe` [testName "Trip"]

  it "doesn't list another user's folder" $ \(db, justin, other) -> do
    photos <- testFolder db justin AtRoot "Photos"
    Nodes.listChildren db other (InFolder photos.nodeId) Nothing (testPageSize 10)
      `shouldReturn` Left Nodes.FolderNotFound

  it "doesn't list an unknown folder" $ \(db, justin, _) ->
    Nodes.listChildren db justin (InFolder (NodeId nil)) Nothing (testPageSize 10)
      `shouldReturn` Left Nodes.FolderNotFound

  it "creates a folder inside one of the user's folders" $ \(db, justin, _) -> do
    photos <- testFolder db justin AtRoot "Photos"
    Right trip <- Nodes.createFolder db justin (InFolder photos.nodeId) (testName "Trip")
    trip.location `shouldBe` InFolder photos.nodeId

  it "passes a taken name through as NameTaken" $ \(db, justin, _) -> do
    _ <- testFolder db justin AtRoot "Photos"
    Nodes.createFolder db justin AtRoot (testName "Photos") `shouldReturn` Left Nodes.NameTaken

  it "refuses an unknown parent" $ \(db, justin, _) ->
    Nodes.createFolder db justin (InFolder (NodeId nil)) (testName "Trip")
      `shouldReturn` Left Nodes.ParentNotFound

  it "refuses a parent that belongs to another user" $ \(db, justin, other) -> do
    photos <- testFolder db justin AtRoot "Photos"
    Nodes.createFolder db other (InFolder photos.nodeId) (testName "Planted")
      `shouldReturn` Left Nodes.ParentNotFound

-- | The root's names, page by page (2 per page), following each page's cursor.
allPages :: Database.Handle -> UserId -> Maybe Cursor -> IO [[NodeName]]
allPages db owner after = do
  Right page <- Nodes.listChildren db owner AtRoot after (testPageSize 2)
  rest <- maybe (pure []) (allPages db owner . Just) page.next
  pure (map name page.items : rest)
