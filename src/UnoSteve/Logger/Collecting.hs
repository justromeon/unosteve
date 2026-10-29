module UnoSteve.Logger.Collecting
  ( new
  ) where

import Data.IORef (IORef, atomicModifyIORef')
import Data.Text (Text)

import UnoSteve.Logger qualified as Logger

{- | Appends every line to the given IORef, oldest first. For tests: read the IORef to
see what was logged.
-}
new :: IORef [(Logger.Level, Text)] -> Logger.Handle
new ref = Logger.Handle $ \level msg ->
  atomicModifyIORef' ref (\entries -> (entries <> [(level, msg)], ()))
