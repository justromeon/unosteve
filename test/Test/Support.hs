module Test.Support
  ( postgresSpec
  , testEmail
  , testUser
  , testPassword
  , testHasherConfig
  , t0
  , setDay
  , sessionConfig
  , testName
  , testPageSize
  , testFolder
  ) where

import Data.IORef (IORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime (UTCTime), addUTCTime, fromGregorian, nominalDay)
import Hasql.Session qualified as Session
import System.Environment (lookupEnv)
import Test.Hspec

import UnoSteve.Database qualified as Database
import UnoSteve.Database.Nodes qualified as Database.Nodes
import UnoSteve.Domain.Node (Location, Node, NodeName, mkNodeName)
import UnoSteve.Domain.Page (PageSize, mkPageSize)
import UnoSteve.Domain.Password (Password, mkPassword)
import UnoSteve.Domain.User (Email, NewUser (..), PasswordHash (..), UserId, mkEmail)
import UnoSteve.PasswordHasher qualified as PasswordHasher
import UnoSteve.Secret (Secret (..))
import UnoSteve.Sessions qualified as Sessions

-- Each test gets a fresh pool and empty tables. Pending when TEST_DATABASE_URL is unset.
postgresSpec :: SpecWith Database.Handle -> Spec
postgresSpec spec = do
  testDatabaseUrl <- runIO (lookupEnv "TEST_DATABASE_URL")
  case testDatabaseUrl of
    Nothing -> it "needs Postgres" $ pendingWith "set TEST_DATABASE_URL to run"
    Just url -> flip around spec $ \test ->
      Database.withHandle (Database.Config (Secret (T.pack url)) 2) $ \db -> do
        Database.runOrThrow db (Session.sql "TRUNCATE users, sessions, nodes")
        test db

testEmail :: Text -> Email
testEmail raw = either (error . T.unpack) id (mkEmail raw)

testUser :: Text -> NewUser
testUser raw = NewUser (testEmail raw) (PasswordHash "fake-hash")

testPassword :: Text -> Password
testPassword raw = either (error . T.unpack) id (mkPassword raw)

-- Real argon2 at the lowest cost it allows (8 KiB per thread, 1 pass): milliseconds
-- instead of ~0.1 s per hash.
testHasherConfig :: PasswordHasher.Config
testHasherConfig = PasswordHasher.Config{PasswordHasher.memoryKiB = 8, PasswordHasher.iterations = 1}

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 1 1) 0

-- | Move the test clock to n days after t0.
setDay :: IORef UTCTime -> NominalDiffTime -> IO ()
setDay clockRef n = writeIORef clockRef (addUTCTime (n * nominalDay) t0)

sessionConfig :: Sessions.Config
sessionConfig = Sessions.Config{Sessions.ttl = 7 * nominalDay, Sessions.maxAge = 30 * nominalDay}

testName :: Text -> NodeName
testName raw = either (error . T.unpack) id (mkNodeName raw)

testPageSize :: Int -> PageSize
testPageSize n = either (error . T.unpack) id (mkPageSize n)

testFolder :: Database.Handle -> UserId -> Location -> Text -> IO Node
testFolder db owner place raw =
  Database.Nodes.createFolder db owner place (testName raw) >>= either (fail . show) pure
