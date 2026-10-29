module UnoSteve.Clock.Settable
  ( new
  ) where

import Data.IORef (IORef, readIORef)
import Data.Time (UTCTime)
import UnoSteve.Clock qualified as Clock

new :: IORef UTCTime -> Clock.Handle
new ref = Clock.Handle (readIORef ref)
