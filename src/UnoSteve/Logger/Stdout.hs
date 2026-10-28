module UnoSteve.Logger.Stdout
  ( Config (..)
  , new
  , close
  , withHandle
  ) where

import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (bracket)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import System.IO (BufferMode (LineBuffering), hFlush, hSetBuffering, stdout)

import UnoSteve.Logger qualified as Logger

newtype Config = Config {minLevel :: Logger.Level}
  deriving (Show)

new :: Config -> IO Logger.Handle
new config = do
  hSetBuffering stdout LineBuffering
  lock <- newMVar ()
  let write level msg = withMVar lock $ \() -> do
        now <- getCurrentTime
        TIO.putStrLn $ T.pack (iso8601Show now) <> " [" <> Logger.renderLevel level <> "] " <> msg
  pure $ Logger.atLeast config.minLevel (Logger.Handle write)

close :: Logger.Handle -> IO ()
close _ = hFlush stdout

withHandle :: Config -> (Logger.Handle -> IO a) -> IO a
withHandle config = bracket (new config) close
