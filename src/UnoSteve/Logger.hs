module UnoSteve.Logger where

import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import System.IO (hSetBuffering, stdout, BufferMode (LineBuffering), hFlush)

data Level
  = Debug
  | Info
  | Warn
  | Error
  deriving (Eq, Ord, Show)

newtype Config = Config { minLevel :: Level }

newtype Handle = Handle
  { logAt :: Level -> Text -> IO ()
  }

new :: Config -> IO Handle
new config = do
  hSetBuffering stdout LineBuffering
  lock <- newMVar ()
  let write level msg = withMVar lock $ \() -> do
         now <- getCurrentTime
         TIO.putStrLn $ T.pack (iso8601Show now) <> " [" <> renderLevel level <> "] " <> msg
  pure $ atLeast config.minLevel (Handle write)

close :: Handle -> IO ()
close _ = hFlush stdout

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config = bracket (new config) close

atLeast :: Level -> Handle -> Handle
atLeast threshold h = Handle $ \level msg ->
  when (level >= threshold) (h.logAt level msg)

withPrefix :: Text -> Handle -> Handle
withPrefix prefix h = Handle $ \level msg -> h.logAt level (prefix <> msg)

renderLevel :: Level -> Text
renderLevel Debug = "DEBUG"
renderLevel Info  = "INFO"
renderLevel Warn  = "WARN"
renderLevel Error = "ERROR"

parseLevel :: Text -> Either Text Level
parseLevel = \case
  "debug" -> Right Debug
  "info"  -> Right Info
  "warn"  -> Right Warn
  "error" -> Right Error
  invalid -> Left $ "Invalid Level: " <> invalid

noop :: Handle
noop = Handle $ \_ _ -> pure ()

logDebug, logInfo, logWarn, logError :: Handle -> Text -> IO ()
logDebug h = h.logAt Debug
logInfo  h = h.logAt Info
logWarn  h = h.logAt Warn
logError h = h.logAt Error
