module Main (main) where

import Data.Text qualified as T
import Network.Wai.Handler.Warp (run)
import Servant

import UnoSteve.Api (api, server)
import UnoSteve.Config (Config (port))
import UnoSteve.Config qualified as Config
import UnoSteve.Database qualified as Database
import UnoSteve.Env (Env (Env))
import UnoSteve.Logger qualified as Logger
import UnoSteve.UserRepo.Postgres qualified as UserRepoPostgres

main :: IO ()
main = do
  config <- Config.loadConfig
  Logger.withHandle (Logger.Config config.logLevel) $ \logger ->
    Database.withHandle (Database.Config config.databaseUrl 10) $ \database ->
      UserRepoPostgres.withHandle database $ \users -> do
        Logger.logInfo logger $ "Listening on port: " <> T.pack (show config.port)
        run config.port $ serve api $ server (Env config logger database users)
