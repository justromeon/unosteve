module UnoSteve.Sessions
  ( Config (..)
  , Handle
  , new
  , start
  , authenticate
  , end
  ) where

import Crypto.Hash (SHA256 (..), hashWith)
import Crypto.Random (getRandomBytes)
import Data.ByteArray (convert)
import Data.ByteArray.Encoding (Base (Base64URLUnpadded), convertToBase)
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Time (NominalDiffTime, addUTCTime, diffUTCTime)

import UnoSteve.Clock qualified as Clock
import UnoSteve.Database qualified as Database
import UnoSteve.Database.Sessions qualified as Database.Sessions
import UnoSteve.Domain.Session (Session (..), SessionToken (..), tokenText)
import UnoSteve.Domain.User (UserId)

data Config = Config
  { ttl :: NominalDiffTime
  -- ^ How long an unused session lives.
  , maxAge :: NominalDiffTime
  -- ^ Absolute limit of how long a session lives
  }
  deriving stock (Show)

data Handle = Handle
  { config :: Config
  , database :: Database.Handle
  , clock :: Clock.Handle
  }

-- | Borrows the database and the clock; owns nothing, so there's no close.
new :: Config -> Database.Handle -> Clock.Handle -> Handle
new cfg db clk = Handle{config = cfg, database = db, clock = clk}

-- | Start a session for this user. The token is returned once and never stored.
start :: Handle -> UserId -> IO (SessionToken, Session)
start h uid = do
  token <- generateToken
  now <- h.clock.now
  session <-
    Database.Sessions.create h.database (hashToken token) uid now (addUTCTime h.config.ttl now)
  pure (token, session)

{- | The session behind this token, if it exists and hasn't expired.
Renews the expiry once the session is past half its lifetime.
-}
authenticate :: Handle -> SessionToken -> IO (Maybe Session)
authenticate h token = do
  found <- Database.Sessions.findByTokenHash h.database (hashToken token)
  now <- h.clock.now
  case found of
    Nothing -> pure Nothing
    Just session
      | session.expiresAt <= now || absoluteExpiry <= now -> do
          end h session
          pure Nothing
      | dueForRenewal && renewed > session.expiresAt -> do
          Database.Sessions.setExpiresAt h.database session.sessionId renewed
          pure (Just session{expiresAt = renewed})
      | otherwise -> pure (Just session)
     where
      absoluteExpiry = addUTCTime h.config.maxAge session.createdAt
      -- Past half its idle lifetime: renew now, rather than on every request.
      dueForRenewal = diffUTCTime session.expiresAt now < h.config.ttl / 2
      -- Slide the idle expiry forward, but never past the absolute one.
      renewed = min (addUTCTime h.config.ttl now) absoluteExpiry

-- | Logout: the token stops working immediately.
end :: Handle -> Session -> IO ()
end h session = Database.Sessions.delete h.database session.sessionId

-- | 32 bytes from the operating system's secure random generator, base64url-encoded.
generateToken :: IO SessionToken
generateToken = do
  bytes <- getRandomBytes 32 :: IO ByteString
  pure (SessionToken (decodeUtf8Lenient (convertToBase Base64URLUnpadded bytes)))

hashToken :: SessionToken -> ByteString
hashToken token = convert (hashWith SHA256 (encodeUtf8 (tokenText token)))
