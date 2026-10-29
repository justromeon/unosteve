module UnoSteve.Clock.System
  ( new
  ) where

import Data.Time (getCurrentTime)

import UnoSteve.Clock qualified as Clock

new :: Clock.Handle
new = Clock.Handle getCurrentTime
