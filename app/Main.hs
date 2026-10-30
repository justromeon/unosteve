module Main (main) where

import Data.Text qualified as T
import Network.Wai.Handler.Warp (run)

import UnoSteve.Api (app)
import UnoSteve.Clock.System qualified as Clock.System
import UnoSteve.Config qualified as Config
import UnoSteve.Database qualified as Database
import UnoSteve.Env (Env (Env))
import UnoSteve.Logger qualified as Logger
import UnoSteve.Logger.Stdout qualified as Logger.Stdout
import UnoSteve.PasswordHasher qualified as PasswordHasher
import UnoSteve.Sessions qualified as Sessions

main :: IO ()
main = do
  config <- Config.load
  Logger.Stdout.withHandle config.logger $ \logger ->
    Database.withHandle config.database $ \database -> do
      hasher <- PasswordHasher.new PasswordHasher.defaultConfig
      let sessions = Sessions.new config.sessions database Clock.System.new
      Logger.logInfo logger $ "Listening on port: " <> T.pack (show config.port)
      run config.port $ app (Env logger database hasher sessions)
