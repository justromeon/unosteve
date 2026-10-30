module Main (main) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Hasql.Session qualified as Session
import System.Environment (lookupEnv)
import Test.Hspec

import Data.Either (isLeft, isRight)
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian, nominalDay)

import UnoSteve.Auth qualified as Auth
import UnoSteve.Clock.Fixed qualified as Clock.Fixed
import UnoSteve.Clock.Settable qualified as Clock.Settable
import UnoSteve.Database qualified as Database
import UnoSteve.Database.Users qualified as Database.Users
import UnoSteve.Domain.Password (Password, mkPassword)
import UnoSteve.Domain.Session (SessionToken (SessionToken))
import UnoSteve.Domain.Session qualified as Domain.Sessions
import UnoSteve.Domain.User
  ( Email
  , NewUser (..)
  , PasswordHash (..)
  , User (..)
  , UserId
  , emailText
  , hashText
  , mkEmail
  )
import UnoSteve.Logger qualified as Logger
import UnoSteve.Logger.Collecting qualified as Logger.Collecting
import UnoSteve.PasswordHasher qualified as PasswordHasher
import UnoSteve.Secret (Secret (..))
import UnoSteve.Sessions qualified as Sessions

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
    describe "Sessions" $
      postgresSpec (beforeWith sessionsSetup sessionsSpec)
    describe "Auth" $
      postgresSpec (beforeWith authSetup authSpec)

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
    PasswordHasher.verify hasher "unosteve timing-equalization dummy" Nothing
      `shouldBe` False

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 1 1) 0

sessionConfig :: Sessions.Config
sessionConfig = Sessions.Config{Sessions.ttl = 7 * nominalDay}

-- Each test gets a clock it controls, a sessions handle, and a real user to log in.
sessionsSetup :: Database.Handle -> IO (IORef UTCTime, Sessions.Handle, UserId)
sessionsSetup db = do
  clockRef <- newIORef t0
  let sessions = Sessions.new sessionConfig db (Clock.Settable.new clockRef)
  Right justin <- Database.Users.create db (testUser "justin@example.com")
  pure (clockRef, sessions, justin.userId)

sessionsSpec :: SpecWith (IORef UTCTime, Sessions.Handle, UserId)
sessionsSpec = do
  it "authenticates a token it started" $ \(_, sessions, justin) -> do
    (token, _) <- Sessions.start sessions justin
    fmap Domain.Sessions.userId <$> Sessions.authenticate sessions token `shouldReturn` Just justin

  it "rejects an unknown token" $ \(_, sessions, _) ->
    Sessions.authenticate sessions (SessionToken "not-a-real-token") `shouldReturn` Nothing

  it "rejects an expired session and deletes it" $ \(clockRef, sessions, justin) -> do
    (token, _) <- Sessions.start sessions justin
    modifyIORef' clockRef (addUTCTime (7 * nominalDay))
    Sessions.authenticate sessions token `shouldReturn` Nothing
    -- Back in time: had the row survived, the token would be valid again.
    writeIORef clockRef t0
    Sessions.authenticate sessions token `shouldReturn` Nothing

  it "leaves a fresh session's expiry alone" $ \(clockRef, sessions, justin) -> do
    (token, started) <- Sessions.start sessions justin
    modifyIORef' clockRef (addUTCTime nominalDay)
    fmap (.expiresAt) <$> Sessions.authenticate sessions token `shouldReturn` Just started.expiresAt

  it "renews a session past half its lifetime" $ \(clockRef, sessions, justin) -> do
    (token, _) <- Sessions.start sessions justin
    modifyIORef' clockRef (addUTCTime (4 * nominalDay))
    _ <- Sessions.authenticate sessions token
    -- A second read shows the renewal was stored, not just returned.
    fmap (.expiresAt) <$> Sessions.authenticate sessions token
      `shouldReturn` Just (addUTCTime (11 * nominalDay) t0)

  it "ends a session" $ \(_, sessions, justin) -> do
    (token, session) <- Sessions.start sessions justin
    Sessions.end sessions session
    Sessions.authenticate sessions token `shouldReturn` Nothing

authSetup :: Database.Handle -> IO (Database.Handle, PasswordHasher.Handle, Sessions.Handle)
authSetup db = do
  hasher <- PasswordHasher.new testHasherConfig
  let sessions = Sessions.new sessionConfig db (Clock.Fixed.new t0)
  pure (db, hasher, sessions)

authSpec :: SpecWith (Database.Handle, PasswordHasher.Handle, Sessions.Handle)
authSpec = do
  describe "register" $ do
    it "stores an argon2 hash, never the plain password" $ \(db, hasher, _) -> do
      Right user <- Auth.register hasher db justin correctHorse
      T.isPrefixOf "$argon2id$" (hashText user.passwordHash) `shouldBe` True

    it "rejects a duplicate email" $ \(db, hasher, _) -> do
      _ <- Auth.register hasher db justin correctHorse
      Auth.register hasher db (testEmail "Justin@Example.com") (testPassword "another long password")
        `shouldReturn` Left Database.Users.EmailTaken

  describe "login" $ do
    it "starts a session for the right password" $ \(db, hasher, sessions) -> do
      Right user <- Auth.register hasher db justin correctHorse
      Right (token, _) <- Auth.login hasher db sessions justin "correct horse battery"
      fmap (.userId) <$> Sessions.authenticate sessions token `shouldReturn` Just user.userId

    it "gives every failure the same error" $ \(db, hasher, sessions) -> do
      _ <- Auth.register hasher db justin correctHorse
      let attempts =
            [ (justin, "wrong horse battery") -- wrong password
            , (testEmail "nobody@example.com", "correct horse battery") -- unknown email
            , (justin, "") -- empty password: checked, not rejected by policy
            ]
      results <- traverse (uncurry (Auth.login hasher db sessions)) attempts
      -- Note: SessionToken has no Eq instance
      [either Just (const Nothing) r | r <- results]
        `shouldBe` replicate (length attempts) (Just Auth.InvalidCredentials)
 where
  justin = testEmail "justin@example.com"
  correctHorse = testPassword "correct horse battery"
