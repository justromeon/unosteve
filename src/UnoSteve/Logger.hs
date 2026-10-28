module UnoSteve.Logger
  ( Level (..)
  , parseLevel
  , renderLevel
  , Handle (..)
  , noop
  , atLeast
  , withPrefix
  , logDebug
  , logInfo
  , logWarn
  , logError
  ) where

import Control.Monad (when)
import Data.Text (Text)

data Level
  = Debug
  | Info
  | Warn
  | Error
  deriving (Eq, Ord, Show)

renderLevel :: Level -> Text
renderLevel Debug = "DEBUG"
renderLevel Info = "INFO"
renderLevel Warn = "WARN"
renderLevel Error = "ERROR"

parseLevel :: Text -> Either Text Level
parseLevel = \case
  "debug" -> Right Debug
  "info" -> Right Info
  "warn" -> Right Warn
  "error" -> Right Error
  invalid -> Left $ "Invalid Level: " <> invalid

newtype Handle = Handle
  { logAt :: Level -> Text -> IO ()
  }

noop :: Handle
noop = Handle $ \_ _ -> pure ()

atLeast :: Level -> Handle -> Handle
atLeast threshold h = Handle $ \level msg ->
  when (level >= threshold) (h.logAt level msg)

withPrefix :: Text -> Handle -> Handle
withPrefix prefix h = Handle $ \level msg -> h.logAt level (prefix <> msg)

logDebug, logInfo, logWarn, logError :: Handle -> Text -> IO ()
logDebug h = h.logAt Debug
logInfo h = h.logAt Info
logWarn h = h.logAt Warn
logError h = h.logAt Error
