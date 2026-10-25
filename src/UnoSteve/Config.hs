module UnoSteve.Config where

import Control.Monad (unless)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read (decimal)
import System.Environment (getEnvironment)
import System.Exit (die)

import UnoSteve.Logger qualified as Logger

newtype Secret = Secret { reveal :: Text }

instance Show Secret where
  show _ = "<redacted>"

data Config = Config
  { port :: Int
  , databaseUrl :: Secret
  , logLevel :: Logger.Level
  , s3 :: S3Config
  } deriving Show

data S3Config = S3Config
  { endpoint :: Text
  , region :: Text
  , bucket :: Text
  , accessKey :: Text
  , secretKey :: Secret
  } deriving Show

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

loadConfig :: IO Config
loadConfig = do
  env <- Map.map T.pack . Map.fromList <$> getEnvironment

  let missingVars = filter (`Map.notMember` env) requiredVars
  unless (null missingVars) (die $ "Missing required .env vars: " <> show missingVars)

  let getVar :: String -> Either Text Text
      getVar name = maybe
        (Left $ "Missing var: " <> T.pack name)
        Right
        $ Map.lookup name env

  let eitherS3Config = S3Config
        <$> getVar "S3_ENDPOINT"
        <*> getVar "S3_REGION"
        <*> getVar "S3_BUCKET"
        <*> getVar "S3_ACCESS_KEY"
        <*> (Secret <$> getVar "S3_SECRET_KEY")

  let eitherConfig = Config
        <$> (getVar "PORT" >>= parsePort)
        <*> (Secret <$> getVar "DATABASE_URL")
        <*> (getVar "LOG_LEVEL" >>= Logger.parseLevel)
        <*> eitherS3Config

  case eitherConfig of
    Left err -> die $ T.unpack err
    Right c  -> pure c
