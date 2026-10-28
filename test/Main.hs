module Main (main) where

import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Hasql.Session qualified as Session
import System.Environment (lookupEnv)
import Test.Hspec

import UnoSteve.Database qualified as Database
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
import UnoSteve.UserRepo qualified as UserRepo
import UnoSteve.UserRepo.InMemory qualified as UserRepo.InMemory
import UnoSteve.UserRepo.Postgres qualified as UserRepo.Postgres

testEmail :: Text -> Email
testEmail raw = either (error . T.unpack) id (mkEmail raw)

testUser :: Text -> NewUser
testUser raw = NewUser (testEmail raw) (PasswordHash "fake-hash")

main :: IO ()
main = do
  testDatabaseUrl <- lookupEnv "TEST_DATABASE_URL"
  hspec $ do
    loggerSpec
    emailSpec

    describe "UserRepo (InMemory)" $
      around UserRepo.InMemory.withHandle userRepoContract

    describe "UserRepo (Postgres)" $ case testDatabaseUrl of
      Nothing -> it "runs against Postgres" $ pendingWith "set TEST_DATABASE_URL to run"
      Just url -> around (withPostgres url) userRepoContract

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
userRepoContract :: SpecWith UserRepo.Handle
userRepoContract = do
  it "finds a user it created using email" $ \repo -> do
    Right created <- repo.create (testUser "justin@example.com")
    repo.findByEmail (testEmail "justin@example.com") `shouldReturn` Just created

  it "finds a user it created using id" $ \repo -> do
    Right created <- repo.create (testUser "justin@example.com")
    repo.findById created.userId `shouldReturn` Just created

  it "rejects a duplicate email, ignoring case" $ \repo -> do
    _ <- repo.create (testUser "justin@example.com")
    repo.create (testUser "Justin@Example.COM") `shouldReturn` Left UserRepo.EmailTaken

  it "returns Nothing for an unknown email" $ \repo -> do
    repo.findByEmail (testEmail "unknown@email.com") `shouldReturn` Nothing

-- | Fresh pool and empty table for every test.
withPostgres :: String -> (UserRepo.Handle -> IO ()) -> IO ()
withPostgres url action =
  Database.withHandle (Database.Config (Secret (T.pack url)) 2) $ \db -> do
    Database.runOrThrow db (Session.sql "TRUNCATE users")
    UserRepo.Postgres.withHandle db action
