module UnoSteve.SessionsSpec (spec) where

import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef', newIORef, writeIORef)
import Data.Time (UTCTime, addUTCTime, nominalDay)
import Test.Hspec

import Test.Support (postgresSpec, sessionConfig, setDay, t0, testUser)
import UnoSteve.Clock.Settable qualified as Clock.Settable
import UnoSteve.Database qualified as Database
import UnoSteve.Database.Users qualified as Database.Users
import UnoSteve.Domain.Session (SessionToken (SessionToken))
import UnoSteve.Domain.Session qualified as Domain.Sessions
import UnoSteve.Domain.User (User (..), UserId)
import UnoSteve.Sessions qualified as Sessions

spec :: Spec
spec = postgresSpec (beforeWith setup sessionsSpec)

-- Each test gets a clock it controls, a sessions handle, and a real user to log in.
setup :: Database.Handle -> IO (IORef UTCTime, Sessions.Handle, UserId)
setup db = do
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

  it "ends a session at its absolute expiry, however active it is" $ \(clockRef, sessions, justin) -> do
    (token, _) <- Sessions.start sessions justin
    -- Used every 4 days: each use is within the idle limit, so it stays alive...
    for_ [4, 8, 12, 16, 20, 24, 28] $ \n -> do
      setDay clockRef n
      fmap (.userId) <$> Sessions.authenticate sessions token `shouldReturn` Just justin
    -- ...until day 30, 30 days after login.
    setDay clockRef 30
    Sessions.authenticate sessions token `shouldReturn` Nothing
    -- Back to day 29: had the row survived, the token would be valid again.
    setDay clockRef 29
    Sessions.authenticate sessions token `shouldReturn` Nothing

  it "never renews past the absolute expiry" $ \(clockRef, sessions, justin) -> do
    (token, _) <- Sessions.start sessions justin
    for_ [4, 8, 12, 16, 20] $ \n -> do
      setDay clockRef n
      Sessions.authenticate sessions token
    -- Due for renewal on day 24: a full TTL would reach day 31, but the cap is day 30.
    setDay clockRef 24
    fmap (.expiresAt) <$> Sessions.authenticate sessions token
      `shouldReturn` Just (addUTCTime (30 * nominalDay) t0)

  it "ends every session of a user" $ \(_, sessions, justin) -> do
    (laptop, _) <- Sessions.start sessions justin
    (phone, _) <- Sessions.start sessions justin
    Sessions.endAll sessions justin
    Sessions.authenticate sessions laptop `shouldReturn` Nothing
    Sessions.authenticate sessions phone `shouldReturn` Nothing
