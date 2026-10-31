module UnoSteve.Config
  ( Config (..)
  , load
  ) where

import Control.Monad (unless)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read (decimal)
import System.Environment (getEnvironment)
import System.Exit (die)

import Data.Time (NominalDiffTime, nominalDay)
import UnoSteve.Database qualified as Database
import UnoSteve.Logger qualified as Logger
import UnoSteve.Logger.Stdout qualified as Logger.Stdout
import UnoSteve.Secret (Secret (..))
import UnoSteve.Sessions qualified as Sessions
import UnoSteve.Storage.R2 qualified as Storage.R2

data Config = Config
  { port :: Int
  , logger :: Logger.Stdout.Config
  , database :: Database.Config
  , sessions :: Sessions.Config
  , storage :: Storage.R2.Config
  }
  deriving (Show)

requiredVars :: [String]
requiredVars =
  [ "PORT"
  , "DATABASE_URL"
  , "S3_ENDPOINT"
  , "S3_REGION"
  , "S3_BUCKET"
  , "S3_ACCESS_KEY"
  , "S3_SECRET_KEY"
  ]

parsePort :: Text -> Either Text Int
parsePort t = case decimal t of
  Right (p, "") | p > 0 && p < 65536 -> Right p
  _ -> Left $ "PORT must be a number between 1 and 65535, got: " <> t

parseSeconds :: Text -> Text -> Either Text NominalDiffTime
parseSeconds name t = case decimal t of
  Right (n :: Integer, "") | n > 0 -> Right (fromInteger n)
  _ -> Left $ name <> " must be a positive number of seconds, got: " <> t

load :: IO Config
load = do
  env <- Map.map T.pack . Map.fromList <$> getEnvironment

  let missingVars = filter (`Map.notMember` env) requiredVars
  unless (null missingVars) (die $ "Missing required .env vars: " <> show missingVars)

  let getVar :: String -> Either Text Text
      getVar name =
        maybe
          (Left $ "Missing var: " <> T.pack name)
          Right
          $ Map.lookup name env

  let eitherS3Config =
        Storage.R2.Config
          <$> (getVar "S3_ENDPOINT" >>= Storage.R2.parseEndpoint)
          <*> getVar "S3_REGION"
          <*> getVar "S3_BUCKET"
          <*> getVar "S3_ACCESS_KEY"
          <*> (Secret <$> getVar "S3_SECRET_KEY")
          <*> pure (15 * 60) -- upload URLs live 15 minutes
          <*> pure (5 * 60) -- download URLs live 5 minutes
  let eitherLoggerConfig =
        Logger.Stdout.Config
          <$> maybe (Right Logger.Info) Logger.parseLevel (Map.lookup "LOG_LEVEL" env)

  let eitherSessionsConfig =
        Sessions.Config
          <$> maybe (Right $ 7 * nominalDay) (parseSeconds "SESSION_TTL") (Map.lookup "SESSION_TTL" env)
          <*> maybe (Right $ 30 * nominalDay) (parseSeconds "SESSION_MAX_AGE") (Map.lookup "SESSION_MAX_AGE" env)

  let eitherDatabaseConfig =
        (Database.Config . Secret <$> getVar "DATABASE_URL")
          <*> pure 10

  let eitherConfig =
        Config
          <$> (getVar "PORT" >>= parsePort)
          <*> eitherLoggerConfig
          <*> eitherDatabaseConfig
          <*> eitherSessionsConfig
          <*> eitherS3Config

  case eitherConfig of
    Left err -> die $ T.unpack err
    Right c -> pure c
