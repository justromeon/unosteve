module UnoSteve.UserRepo.InMemory where
import qualified UnoSteve.UserRepo as UserRepo
import Data.IORef (newIORef, IORef, atomicModifyIORef', readIORef)
import qualified Data.Map as Map
import UnoSteve.User
import Data.Map (Map)
import Data.UUID.V4 (nextRandom)
import Data.Time (getCurrentTime)
import Control.Exception (bracket)
import Data.List (find)

new :: IO UserRepo.Handle
new = do
  ref <- newIORef Map.empty
  pure UserRepo.Handle
    { UserRepo.create = create ref
    , UserRepo.findByEmail = findByEmail ref
    , UserRepo.findById = findbyId ref
    }

close :: UserRepo.Handle -> IO ()
close _ = pure ()

withHandle :: (UserRepo.Handle -> IO a) -> IO a
withHandle = bracket new close

create :: IORef (Map Email User) -> NewUser -> IO (Either UserRepo.CreateError User)
create ref newUser = do
  uuid <- nextRandom
  now <- getCurrentTime
  let user = User
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

findbyId :: IORef (Map Email User) -> UserId -> IO (Maybe User)
findbyId ref targerId = find ((== targerId) . userId) . Map.elems <$> readIORef ref
