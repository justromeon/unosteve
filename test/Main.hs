module Main (main) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Hasql.Session qualified as Session
import System.Environment (lookupEnv)
import Test.Hspec

import UnoSteve.Database qualified as Database
import UnoSteve.Database.Users qualified as Database.Users
import UnoSteve.Domain.User
  ( Email
  , NewUser (..)
  , PasswordHash (..)
  , User (..)
  , emailText
  , mkEmail
  )
import UnoSteve.Logger qualified as Logger
import UnoSteve.Secret (Secret (..))

testEmail :: Text -> Email
testEmail raw = either (error . T.unpack) id (mkEmail raw)

testUser :: Text -> NewUser
testUser raw = NewUser (testEmail raw) (PasswordHash "fake-hash")

main :: IO ()
main = do
  hspec $ do
    loggerSpec
    emailSpec
    describe "Database.Users" $ postgresSpec usersSpec

-- Logger tests
loggerSpec :: Spec
loggerSpec = do
  describe "Logger.atLeast" $ do
    it "drops a message below threshold" $ do
      ref <- newIORef []
      let collecting = Logger.Handle $ \level msg -> modifyIORef' ref ((level, msg) :)
          logger = Logger.atLeast Logger.Warn collecting

      Logger.logDebug logger "hidden"
      Logger.logError logger "shown"
      readIORef ref `shouldReturn` [(Logger.Error, "shown")]

    it "keeps a message within threshold" $ do
      ref <- newIORef []
      let collecting = Logger.Handle $ \level msg -> modifyIORef' ref ((level, msg) :)
          logger = Logger.atLeast Logger.Debug collecting

      Logger.logDebug logger "shown"
      Logger.logError logger "shown"
      readIORef ref `shouldReturn` [(Logger.Error, "shown"), (Logger.Debug, "shown")]

  describe "Logger.withPrefix" $ do
    it "prepends a prefix on logs" $ do
      ref <- newIORef []
      let collecting = Logger.Handle $ \level msg -> modifyIORef' ref ((level, msg) :)
          logger = Logger.withPrefix "[api] " collecting

      Logger.logInfo logger "prefixed log test"
      readIORef ref `shouldReturn` [(Logger.Info, "[api] prefixed log test")]

-- Email parser tests
emailSpec :: Spec
emailSpec = do
  describe "mkEmail" $ do
    it "trims and lowercases" $
      emailText <$> mkEmail "  Justin@Example.COM " `shouldBe` Right "justin@example.com"
    it "rejects a missing @" $
      isLeft (mkEmail "justin.example.com") `shouldBe` True
    it "rejects more than one @" $
      isLeft (mkEmail "a@b@c") `shouldBe` True
 where
  isLeft = either (const True) (const False)

-- Database tests
usersSpec :: SpecWith Database.Handle
usersSpec = do
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

-- \| Each test gets a fresh pool and empty tables. Pending when TEST_DATABASE_URL is unset.
postgresSpec :: SpecWith Database.Handle -> Spec
postgresSpec spec = do
  testDatabaseUrl <- runIO (lookupEnv "TEST_DATABASE_URL")
  case testDatabaseUrl of
    Nothing -> it "needs Postgres" $ pendingWith "set TEST_DATABASE_URL to run"
    Just url -> flip around spec $ \test ->
      Database.withHandle (Database.Config (Secret (T.pack url)) 2) $ \db -> do
        Database.runOrThrow db (Session.sql "TRUNCATE users")
        test db
