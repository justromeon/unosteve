module UnoSteve.UserRepo.InMemory
  ( new
  , close
  , withHandle
  ) where

import Control.Exception (bracket)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)

import UnoSteve.Domain.User (Email, NewUser (..), User (..), UserId (..), defaultQuotaBytes)
import UnoSteve.UserRepo qualified as UserRepo

new :: IO UserRepo.Handle
new = do
  ref <- newIORef Map.empty
  pure
    UserRepo.Handle
      { UserRepo.create = create ref
      , UserRepo.findByEmail = findByEmail ref
      , UserRepo.findById = findById ref
      }

close :: UserRepo.Handle -> IO ()
close _ = pure ()

withHandle :: (UserRepo.Handle -> IO a) -> IO a
withHandle = bracket new close

create :: IORef (Map Email User) -> NewUser -> IO (Either UserRepo.CreateError User)
create ref newUser = do
  uuid <- nextRandom
  now <- getCurrentTime
  let user =
        User
          { userId = UserId uuid
          , email = newUser.email
          , passwordHash = newUser.passwordHash
          , quotaBytes = defaultQuotaBytes
          , createdAt = now
          }
  atomicModifyIORef' ref $ \users ->
    if Map.member newUser.email users
      then (users, Left UserRepo.EmailTaken)
      else (Map.insert newUser.email user users, Right user)

findByEmail :: IORef (Map Email User) -> Email -> IO (Maybe User)
findByEmail ref emailAdd = Map.lookup emailAdd <$> readIORef ref

findById :: IORef (Map Email User) -> UserId -> IO (Maybe User)
findById ref targetId = find ((== targetId) . userId) . Map.elems <$> readIORef ref
