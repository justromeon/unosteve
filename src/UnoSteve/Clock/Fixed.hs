module UnoSteve.Clock.Fixed
  ( new
  ) where

import Data.Time (UTCTime)
import UnoSteve.Clock qualified as Clock

new :: UTCTime -> Clock.Handle
new = Clock.Handle . pure
