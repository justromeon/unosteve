module Main (main) where

import Data.Text qualified as T
import Network.Wai.Handler.Warp (run)
import Servant

import UnoSteve.Api (api, server)
import UnoSteve.Config qualified as Config
import UnoSteve.Database qualified as Database
import UnoSteve.Env (Env (Env))
import UnoSteve.Logger qualified as Logger
import UnoSteve.Logger.Stdout qualified as Logger.Stdout

main :: IO ()
main = do
  config <- Config.load
  Logger.Stdout.withHandle config.logger $ \logger ->
    Database.withHandle config.database $ \database -> do
      Logger.logInfo logger $ "Listening on port: " <> T.pack (show config.port)
      run config.port $ serve api $ server (Env config logger database)
