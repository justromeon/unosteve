module UnoSteve.Database
  ( Config (..)
  , Handle
  , new
  , close
  , withHandle
  , UsageError
  , run
  , runOrThrow
  , ping
  , isUniqueViolation
  ) where

import Control.Exception (bracket, onException, throwIO)
import Hasql.Connection.Setting qualified as ConnectionSetting
import Hasql.Connection.Setting.Connection qualified as ConnectionString
import Hasql.Pool (Pool, UsageError)
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as PoolConfig
import Hasql.Session (Session)
import Hasql.Session qualified as Session

import UnoSteve.Secret (Secret (..))

data Config = Config
  { url :: Secret
  , poolSize :: Int
  }
  deriving (Show)

newtype Handle = Handle {pool :: Pool}

new :: Config -> IO Handle
new config = do
  connections <-
    Pool.acquire $
      PoolConfig.settings
        [ PoolConfig.size config.poolSize
        , PoolConfig.acquisitionTimeout 10
        , PoolConfig.staticConnectionSettings
            [ConnectionSetting.connection (ConnectionString.string config.url.reveal)]
        ]

  let handle = Handle connections
  -- Ping once so bad URL fails at startup instead of first request
  -- Release pool before rethrowing
  ping handle `onException` Pool.release connections
  pure handle

close :: Handle -> IO ()
close h = Pool.release h.pool

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config = bracket (new config) close

run :: Handle -> Session a -> IO (Either UsageError a)
run h = Pool.use h.pool

runOrThrow :: Handle -> Session a -> IO a
runOrThrow h session = run h session >>= either throwIO pure

ping :: Handle -> IO ()
ping h = runOrThrow h (Session.sql "SELECT 1")

-- | Postgres error code 23505 = unique_violation.
isUniqueViolation :: UsageError -> Bool
isUniqueViolation = \case
  Pool.SessionUsageError
    (Session.QueryError _ _ (Session.ResultError (Session.ServerError "23505" _ _ _ _))) -> True
  _ -> False
