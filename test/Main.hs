module Main (main) where

import Data.IORef (newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Hasql.Session qualified as Session
import System.Environment (lookupEnv)
import Test.Hspec

import Data.Either (isLeft, isRight)
import UnoSteve.Database qualified as Database
import UnoSteve.Database.Users qualified as Database.Users
import UnoSteve.Domain.Password (Password, mkPassword)
import UnoSteve.Domain.User
  ( Email
  , NewUser (..)
  , PasswordHash (..)
  , User (..)
  , emailText
  , hashText
  , mkEmail
  )
import UnoSteve.Logger qualified as Logger
import UnoSteve.Logger.Collecting qualified as Logger.Collecting
import UnoSteve.PasswordHasher qualified as PasswordHasher
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
    passwordSpec
    passwordHasherSpec

-- Logger tests
loggerSpec :: Spec
loggerSpec = do
  describe "Logger.atLeast" $ do
    it "drops a message below threshold" $ do
      logged <- newIORef []
      let logger = Logger.atLeast Logger.Warn (Logger.Collecting.new logged)
      Logger.logDebug logger "hidden"
      Logger.logError logger "shown"
      readIORef logged `shouldReturn` [(Logger.Error, "shown")]

    it "keeps a message within threshold" $ do
      logged <- newIORef []
      let logger = Logger.atLeast Logger.Debug (Logger.Collecting.new logged)
      Logger.logDebug logger "shown"
      Logger.logError logger "shown"
      readIORef logged `shouldReturn` [(Logger.Debug, "shown"), (Logger.Error, "shown")]

  describe "Logger.withPrefix" $ do
    it "prepends a prefix on logs" $ do
      logged <- newIORef []
      let logger = Logger.withPrefix "[api] " (Logger.Collecting.new logged)
      Logger.logInfo logger "prefixed log test"
      readIORef logged `shouldReturn` [(Logger.Info, "[api] prefixed log test")]

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

-- Each test gets a fresh pool and empty tables. Pending when TEST_DATABASE_URL is unset.
postgresSpec :: SpecWith Database.Handle -> Spec
postgresSpec spec = do
  testDatabaseUrl <- runIO (lookupEnv "TEST_DATABASE_URL")
  case testDatabaseUrl of
    Nothing -> it "needs Postgres" $ pendingWith "set TEST_DATABASE_URL to run"
    Just url -> flip around spec $ \test ->
      Database.withHandle (Database.Config (Secret (T.pack url)) 2) $ \db -> do
        Database.runOrThrow db (Session.sql "TRUNCATE users, sessions")
        test db

-- Auth tests
passwordSpec :: Spec
passwordSpec = describe "mkPassword" $ do
  it "rejects passwords shorter than 10 characters" $
    isLeft (mkPassword "short") `shouldBe` True
  it "accepts a 10-character password" $
    isRight (mkPassword "0123456789") `shouldBe` True
  it "rejects passwords longer than 128 characters" $
    isLeft (mkPassword (T.replicate 129 "a")) `shouldBe` True

testPassword :: Text -> Password
testPassword raw = either (error . T.unpack) id (mkPassword raw)

-- Real argon2 at the lowest cost it allows (8 KiB per thread, 1 pass): milliseconds
-- instead of ~0.1 s per hash.
testHasherConfig :: PasswordHasher.Config
testHasherConfig = PasswordHasher.Config{PasswordHasher.memoryKiB = 8, PasswordHasher.iterations = 1}

passwordHasherSpec :: Spec
passwordHasherSpec = describe "PasswordHasher" $ do
  it "stores the algorithm and its parameters with the hash" $ do
    hasher <- PasswordHasher.new testHasherConfig
    stored <- PasswordHasher.hash hasher (testPassword "correct horse battery")
    T.isPrefixOf "$argon2id$v=19$m=8,t=1,p=1$" (hashText stored) `shouldBe` True

  it "rejects every password when there is no stored hash" $ do
    hasher <- PasswordHasher.new testHasherConfig
    PasswordHasher.verify hasher (testPassword "unosteve timing-equalization dummy") Nothing
      `shouldBe` False
