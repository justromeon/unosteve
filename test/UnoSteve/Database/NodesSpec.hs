module UnoSteve.Database.NodesSpec where

import Test.Hspec (Spec, SpecWith, beforeWith, it, shouldBe, shouldReturn)
import Test.Support (postgresSpec, testFolder, testName, testUser)
import UnoSteve.Database qualified as Database
import UnoSteve.Database.Nodes qualified as Database.Nodes
import UnoSteve.Database.Nodes qualified as Databse.Nodes
import UnoSteve.Database.Users qualified as Database.Users
import UnoSteve.Domain.Node
  ( Location (AtRoot, InFolder)
  , Node (kind, location, nodeId)
  , NodeKind (Folder)
  )
import UnoSteve.Domain.User (User (userId), UserId)

spec :: Spec
spec = postgresSpec (beforeWith setup nodesSpec)

setup :: Database.Handle -> IO (Database.Handle, UserId, UserId)
setup db = do
  Right justin <- Database.Users.create db (testUser "justin@example.com")
  Right other <- Database.Users.create db (testUser "other@example.com")
  pure (db, justin.userId, other.userId)

nodesSpec :: SpecWith (Database.Handle, UserId, UserId)
nodesSpec = do
  it "creates a folder at the root and finds it again" $ \(db, justin, _) -> do
    photos <- testFolder db justin AtRoot "Photos"
    photos.kind `shouldBe` Folder
    photos.location `shouldBe` AtRoot
    Database.Nodes.findById db justin photos.nodeId `shouldReturn` Just photos

  it "creates a folder inside a folder" $ \(db, justin, _) -> do
    photos <- testFolder db justin AtRoot "Photos"
    trip <- testFolder db justin (InFolder photos.nodeId) "Trip"
    trip.location `shouldBe` InFolder photos.nodeId

  it "rejects a duplicate name inside a folder" $ \(db, justin, _) -> do
    photos <- testFolder db justin AtRoot "Photos"
    _ <- testFolder db justin (InFolder photos.nodeId) "Trip"
    Databse.Nodes.createFolder db justin (InFolder photos.nodeId) (testName "Trip")
      `shouldReturn` Left Database.Nodes.NameTaken

  it "rejects a duplicate name at the root (NULLS NOT DISTINCT)" $ \(db, justin, _) -> do
    _ <- testFolder db justin AtRoot "Photos"
    Database.Nodes.createFolder db justin AtRoot (testName "Photos")
      `shouldReturn` Left Database.Nodes.NameTaken

  it "allows the same name in another folder and for another user" $ \(db, justin, other) -> do
    photos <- testFolder db justin AtRoot "Photos"
    _ <- testFolder db justin (InFolder photos.nodeId) "Photos"
    _ <- testFolder db other (InFolder photos.nodeId) "Photos"
    pure ()

  it "hides a node from another user" $ \(db, justin, other) -> do
    photos <- testFolder db justin AtRoot "Photos"
    Database.Nodes.findById db other photos.nodeId `shouldReturn` Nothing
