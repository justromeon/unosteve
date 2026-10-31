module UnoSteve.Storage.InMemory where

import Data.IORef (IORef, atomicModifyIORef', readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (NominalDiffTime, addUTCTime)
import UnoSteve.Clock (Handle (now))
import UnoSteve.Clock qualified as Clock
import UnoSteve.Domain.Node (nodeNameText)
import UnoSteve.Storage (ObjectInfo, StorageKey, expiresAt)
import UnoSteve.Storage qualified as Storage

{- | Objects live in a map the caller owns, so a test "uploads" by writing to it.
The URLs are fake (memory://…) but spell out what was signed, for tests to check.
-}
new :: IORef (Map StorageKey ObjectInfo) -> Clock.Handle -> Storage.Handle
new objects clock =
  Storage.Handle
    { Storage.presignPut = \key mime size ->
        presign
          ("PUT/" <> Storage.storageKeyText key <> "?contentType=" <> mime <> "&size=" <> T.pack (show size))
    , Storage.presignGet = \key filename ->
        presign ("GET/" <> Storage.storageKeyText key <> "?filename=" <> nodeNameText filename)
    , Storage.headObject = \key -> Map.lookup key <$> readIORef objects
    , Storage.deleteObject = \key -> atomicModifyIORef' objects $ \stored -> (Map.delete key stored, ())
    }
 where
  presign path = do
    now <- clock.now
    pure Storage.PresignedUrl{url = "memory://" <> path, expiresAt = addUTCTime validFor now}

validFor :: NominalDiffTime
validFor = 15 * 60
