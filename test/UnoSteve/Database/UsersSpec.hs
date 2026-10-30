module UnoSteve.Database.UsersSpec (spec) where

import Test.Hspec

import Test.Support (postgresSpec, testEmail, testUser)
import UnoSteve.Database.Users qualified as Database.Users
import UnoSteve.Domain.User (User (..))

spec :: Spec
spec = postgresSpec $ do
  it "finds a user it created using email" $ \db -> do
    Right created <- Database.Users.create db (testUser "justin@example.com")
    Database.Users.findByEmail db (testEmail "justin@example.com") `shouldReturn` Just created

  it "finds a user it created using id" $ \db -> do
    Right created <- Database.Users.create db (testUser "justin@example.com")
    Database.Users.findById db created.userId `shouldReturn` Just created

  it "rejects a duplicate email, ignoring case" $ \db -> do
    _ <- Database.Users.create db (testUser "justin@example.com")
    Database.Users.create db (testUser "Justin@Example.COM")
      `shouldReturn` Left Database.Users.EmailTaken

  it "returns Nothing for an unknown email" $ \db ->
    Database.Users.findByEmail db (testEmail "unknown@email.com") `shouldReturn` Nothing
